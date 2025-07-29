# server.R

# 0) Pakete laden
library(shiny)
library(DT)        # für DataTable
library(ggplot2)   # für Plots
library(readxl)    # für read_excel()
library(dplyr)     # für filter, left_join, distinct
library(writexl)   # für Excel-Export


shinyServer(function(input, output, session) {
  
  ## ------------------------------------------
  ## 0) Hilfs-Reaktivität (Material)
  ## ------------------------------------------
  # Material-Liste für Drop-down füllen
  observe({
    req(orders)
    mats <- sort(unique(orders$MATNR))
    updateSelectInput(session, "material",
                      choices  = mats,
                      selected = first(mats))
  })
  
  ## ------------------------------------------
  ## 1) Daten nach Material filtern
  ## ------------------------------------------
  df_filtered <- reactive({
    req(input$material)
    orders %>%
      filter(MATNR == input$material) %>%
      arrange(desc(Lieferdatum))
  })
  
  ## Datensatz pro Material auf die neuesten 25 Einträge begrenzen
  df_last25 <- reactive({
    req(input$material)
    df_filtered() %>%
      slice_head(n = 25)
  })
  
  ## Daten für KPI abhängig von Checkbox
  df_used <- reactive({
    if (!is.null(input$use_all) && input$use_all) {
      df_filtered()
    } else {
      df_last25()
    }
  })
  
  ## Checkbox anzeigen, wenn mehr als 25 Datensätze vorhanden sind
  output$all_data_checkbox <- renderUI({
    if (nrow(df_filtered()) > 25) {
      checkboxInput("use_all", "Use all data", value = FALSE)
    }
  })
  
  ## Kennzahlen für Anzeige "Data Used" und "Datasets Used"
  output$data_range <- renderText({
    df <- df_used()
    if (nrow(df) == 0) return("-")
    paste0(
      "from ", format(min(df$Lieferdatum), "%d.%m.%Y"),
      " to ", format(max(df$Lieferdatum), "%d.%m.%Y")
    )
  })
  
  output$data_count <- renderText({
    nrow(df_used())
  })  
  
  ## ------------------------------------------
  ## 2) KPI-Berechnung (pro Material)
  ## ------------------------------------------
  kpi_vals <- reactive({
    req(input$material)
    po_keys <- df_used() %>% select(EBELN, EBELP)
    
    # IFR-Details einmal berechnen
    ifr_det <- get_ifr_details(
      material_id = input$material,
      master_df   = orders,
      top_n       = if (isTRUE(input$use_all)) NULL else 25,
      target_ifr  = 95,  # z.B. 0.80 oder 0.95 ausprobieren
      min_up_pct  = 5
    )
    
    list(
      ifr_value = ifr_det$item_ifr,
      ifr_flag  = ifr_det$flag,
      ifr_reco  = ifr_det$recommendation,
      ifr_box   = ifr_det$boxplot_vec,
      ifr_time  = ifr_det$timeline_tbl,
      ifr_avg   = ifr_det$avg_ifr_all,
      ifr_best  = max(ifr_det$boxplot_vec, na.rm = TRUE),
      ifr_worst = min(ifr_det$boxplot_vec, na.rm = TRUE),
      
      otdr  = calculate_otdr(input$material, EKET, EKES, EKPO, po_filter = po_keys),
      oct   = calculate_poct(input$material, EKPO, EKKO, EKBE, po_filter = po_keys),
      delay = calculate_mean_delay(input$material, orders, EKET, EKES, po_filter = po_keys),
      otd_company   = calculate_otdr_company(orders, EKET, EKES, po_filter = po_keys),
      delay_company = calculate_mean_delay_company(orders, EKET, EKES, po_filter = po_keys),
      worst_delay   = calculate_worst_delay(input$material, orders, EKET, EKES, po_filter = po_keys),
      delay_box     = boxplot_delay(input$material, orders, EKET, EKES, po_filter = po_keys),
      delay_time    = timeline_delay(input$material, orders, EKET, EKES, po_filter = po_keys)
    )
  })
  
  ## KPI-Cards (Text-Outputs für ui.R)
  output$kpi_ifr <- renderText(sprintf("%.1f %%", kpi_vals()$ifr_value))
  output$kpi_otdr  <- renderText( sprintf("%.1f %%", kpi_vals()$otdr * 100) )
  output$kpi_oct  <- renderText( sprintf("%.1f d",  kpi_vals()$oct) )
  output$kpi_delay  <- renderText( sprintf("%.1f d",  kpi_vals()$delay) )
  
  
  ## IFR-Daten für Detailansicht
  output$ifr_flag <- renderText(kpi_vals()$ifr_flag)
  output$ifr_avg <- renderText(sprintf("Ø IFR (alle Materialien): %.1f %%",
                                       kpi_vals()$ifr_avg))
  output$ifr_reco <- renderText(kpi_vals()$ifr_reco)
  
  output$ifr_boxplot <- renderPlot({
    boxplot(kpi_vals()$ifr_box,
            main = "IFR-Verteilung (letzte Bestellungen)",
            ylab = "IFR %")
  })
  
  output$otd_boxplot <- renderPlot({
    kpi_vals()$delay_box
  })
  
  output$ifr_timeline <- renderPlot({
    df <- kpi_vals()$ifr_time
    ggplot(df, aes(x = Lieferdatum, y = ifr_line)) +
      geom_line() + geom_point() +
      labs(title = "IFR-Zeitreihe", y = "IFR %", x = "Lieferdatum") +
      theme_minimal()
  })
  
  output$otd_timeline <- renderPlot({
    df <- kpi_vals()$delay_time
    if (is.null(df)) return(NULL)
    ggplot(df, aes(x = Lieferdatum, y = delay_days)) +
      geom_line() + geom_point() +
      labs(title = "Delay Timeline", y = "Delay (days)", x = "Lieferdatum") +
      theme_minimal()
  })
  
  ## -------------------------------------------------
  ## 4) Zusätzliche Info 
  ## -------------------------------------------------
  
  # Farbcodes deiner App – ggf. anpassen
  kpi_colors <- list(red  = "#d9534f",
                     yellow = "#f0ad4e",
                     blue  = "#5bc0de",
                     green = "#5cb85c",
                     grey  = "#999999")
  
  get_color_info <- function(value, avg) {
    if (is.na(avg)) {
      list(color = kpi_colors$grey,   desc = "no average available")
    } else if (value < avg * 0.90) {
      list(color = kpi_colors$red,    desc = ">10 % below average")
    } else if (value < avg * 0.95) {
      list(color = kpi_colors$yellow, desc = "5–10 % below average")
    } else if (value <= avg * 1.05) {
      list(color = kpi_colors$blue,   desc = "within ±5 % of average")
    } else {
      list(color = kpi_colors$green,  desc = ">5 % above average")
    }
  }
  
  ## Durchschnittswerte einmalig aus kpi_vals()
  avg_ifr <- reactive(kpi_vals()$avg_ifr_all)            # kommt aus get_ifr_details
  avg_otd <- reactive(mean(kpi_vals()$otdr * 100, na.rm = TRUE))  # Beispiel
  
  selected_kpi  <- reactiveVal(NULL)
  selected_rule <- reactiveVal(NULL)
  plot_state <- reactiveVal("box")  # can be "box" or "time"
  
  observeEvent(input$kpi_ifr, {
    value <- kpi_vals()$ifr_value
    avg   <- avg_ifr()
    
    if (is.numeric(value) && is.numeric(avg)) {
      info <- get_color_info(value, avg)
    } else {
      info <- list(color = kpi_colors$grey, desc = "value or average is not available")
    }
    
    selected_kpi("Item Fill Rate")
    selected_rule(info$desc)
    session$sendCustomMessage("update-bar-color", info$color)
  })
  
  
  observeEvent(input$kpi_time, {   # Button-ID 'kpi_time' muss in ui.R existieren
    value <- kpi_vals()$otdr * 100
    info  <- get_color_info(value, avg_otd())
    selected_kpi ("On-Time Delivery")
    selected_rule(info$desc)
    session$sendCustomMessage("update-bar-color", info$color)
  })
  
  output$kpi_info <- renderUI({
    kpi  <- selected_kpi()
    desc <- selected_rule()
    if (is.null(kpi)) {
      kpi  <- "Please select the field you are interested in"
      desc <- ""
    }

    div(
      style = "display:flex; width:100%;",
      div(style = "flex:1;"),
      div(kpi,  style = "flex:1; text-align:center;"),
      div(desc, style = "flex:1; text-align:right; font-size:0.9em; font-style:italic;")
    )
  })
  
  plot_state <- reactiveVal("box")  # can be "box" or "time"
  
  # Button toggles plot type and label
  observeEvent(input$toggle_plot, {
    new_state <- if (plot_state() == "box") "time" else "box"
    plot_state(new_state)
    
    updateActionButton(session, "toggle_plot",
                       label = if (new_state == "box") "Timeline" else "Variation")
  })
  
  # Dynamic plot output depending on selected KPI and state
  output$kpi_plot <- renderUI({
    kpi <- selected_kpi()
    if (is.null(kpi)) return(NULL)
    
    plot_id <- if (kpi == "Item Fill Rate") {
      if (plot_state() == "box") "ifr_boxplot" else "ifr_timeline"
    } else {
      if (plot_state() == "box") "otd_boxplot" else "otd_timeline"
    }
    
    plotOutput(plot_id)
  })
  
  
  output$kpi_dynamic_ui <- renderUI({
    kpi <- selected_kpi()
    if (is.null(kpi)) return(NULL)
    
    # Left content
    left_col <- switch(kpi,
                       "Item Fill Rate" = column(6,
                                                 div(style = "font-size: 20px; padding: 7px 0;",
                                                     "Item Fill Rate of the Product on average: ",
                                                     sprintf("%.1f %%", kpi_vals()$ifr_value)),
                                                 div(style = "font-size: 20px; padding: 7px 0;",
                                                     "Item Fill Rate of the Company on average: ",
                                                     sprintf("%.1f %%", kpi_vals()$ifr_avg)),
                                                 div(style = "font-size: 20px; padding: 7px 0;",
                                                     "Worst Item Fill Rate of the Product: ",
                                                     sprintf("%.1f %%", kpi_vals()$ifr_worst)),
                                                 div(style = "font-size: 20px; padding: 7px 0;",
                                                     "Best Item Fill Rate of the Product: ",
                                                     sprintf("%.1f %%", kpi_vals()$ifr_best))
                       ),
                       "On-Time Delivery" = column(6,
                                                   div(style = "font-size: 20px; padding: 5px 0;",
                                                       "On Time Delivery Rate of the Product on average: ",
                                                       sprintf("%.1f %%", kpi_vals()$otdr * 100)),
                                                   div(style = "font-size: 20px; padding: 5px 0;",
                                                       "On Time Delivery Rate of the Company on average: ",
                                                       sprintf("%.1f %%", kpi_vals()$otd_company * 100)),
                                                   div(style = "font-size: 20px; padding: 5px 0;",
                                                       "Mean Delay of the Product on average: ",
                                                       sprintf("%.1f d", kpi_vals()$delay)),
                                                   div(style = "font-size: 20px; padding: 5px 0;",
                                                       "Mean Delay Delivery Rate of the Company on average: ",
                                                       sprintf("%.1f d", kpi_vals()$delay_company)),
                                                   div(style = "font-size: 20px; padding: 5px 0;",
                                                       "Worst Delay of the Product: ",
                                                       sprintf("%.1f d", kpi_vals()$worst_delay))
                       ),
                       column(6, div("Unknown KPI"))
    )
    
    # Right content
    right_col <- column(6,
                        uiOutput("kpi_plot"),
                        br(),
                        actionButton("toggle_plot", "Timeline")
    )
    
    fluidRow(left_col, right_col)
  })
  
  
  
})