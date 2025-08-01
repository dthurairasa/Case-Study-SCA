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
  ## 3.5) Kpi-Cards
  ## -------------------------------------------------
  
  # Background color functions
  get_bg_color_ifr = function(value) {
    if (value >= 98) {
      return("#28a745")  # green
    } else if (value >= 95) {
      return("#ffc107")  # yellow
    } else {
      return("#dc3545")  # red
    }
  }
  
  get_bg_color_rr = function(value) {
    if (value <= 2) {
      return("#28a745")  # green
    } else if (value <= 5) {
      return("#ffc107")  # yellow
    } else {
      return("#dc3545")  # red
    }
  }
  
  get_bg_color_otd = function(value) {
    if (value >= 95) {
      return("#28a745")  # green
    } else if (value >= 90) {
      return("#ffc107")  # yellow
    } else {
      return("#dc3545")  # red
    }
  }
  
  get_bg_color_delay = function(value) {
    if (value <= 1) {
      return("#28a745")  # green
    } else if (value <= 3) {
      return("#ffc107")  # yellow
    } else {
      return("#dc3545")  # red
    }
  }
  
  output$kpi_ifr_button <- renderUI({
    kpi_value_ifr <- kpi_vals()$ifr_value
    bg_color_ifr <- get_bg_color_ifr(kpi_value_ifr)
    
    actionButton(
      inputId = "kpi_ifr",
      label = div(
        class = "kpi-card",
        style = paste("background-color:", bg_color_ifr, "; padding:15px; border-radius:8px; color:white;"),
        div(class = "kpi-logo", img(src = "IFR.png", height = 60)),
        div(class = "kpi-number", textOutput("kpi_ifr")),
        div(class = "kpi-name", "Item Fill Rate")
      ),
      style = "background: none; border: none; padding: 0; width: auto; text-align: left;"
    )
  })
  
  output$kpi_rr_button <- renderUI({
    
    kpi_value_rr <- kpi_vals()$ifr_value #Change
    bg_color_rr <- get_bg_color_ifr(kpi_value_rr)
    
    actionButton(
      inputId = "kpi_rr",
      label = div(
        class = "kpi-card",
        style = paste("background-color:", bg_color_rr, "; padding:15px; border-radius:8px; color:white;"),
        div(class = "kpi-logo", img(src = "RR.png", height = 60)),
        div(class = "kpi-number", "2%" ), #Change
        div(class = "kpi-name", "Return Rate")
      ),
      style = "background: none; border: none; padding: 0; width: auto; text-align: left;"
    )
  })
  
  output$kpi_time_button <- renderUI({
    req(kpi_vals())  # Make sure values are available
    
    # Extract actual values
    otdr_val <- kpi_vals()$otdr * 100  # Convert to %
    oct_val <- kpi_vals()$oct
    delay_val <- kpi_vals()$delay
    
    # Determine background colors
    color_otdr <- get_bg_color_otd(otdr_val)
    color_delay <- get_bg_color_delay(delay_val)  # Example: lower delay = better
    
    actionButton(
      inputId = "kpi_time",
      label = div(
        style = "display: flex; gap: 10px;",
        
        div(
          class = "kpi-card",
          style = paste("background-color:", color_otdr, "; padding:15px; border-radius:8px; color:white;"),
          div(class = "kpi-logo", img(src = "OTD.png", height = 60)),
          div(class = "kpi-number", textOutput("kpi_otdr")),
          div(class = "kpi-name", "On-Time Delivery Rate")
        ),
        div(
          class = "kpi-card",
          style = paste("background-color:", color_delay, "; padding:15px; border-radius:8px; color:white;"),
          div(class = "kpi-logo", img(src = "LTD.png", height = 60)),
          div(class = "kpi-number", textOutput("kpi_delay")),
          div(class = "kpi-name", "Mean Delay")
        )
      ),
      style = "background: none; border: none; padding: 0; width: auto; text-align: left; display: flex;"
    )
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

  selected_kpi  = reactiveVal(NULL)
  selected_rule = reactiveVal(NULL)
  
  observeEvent(input$kpi_ifr, {
    selected_kpi("Item Fill Rate")
  })
  
  observeEvent(input$kpi_rr, {
    selected_kpi("Return Rate")
  })
  
  observeEvent(input$kpi_time, {  
    selected_kpi ("On-Time Delivery")
  })
  
  output$kpi_info <- renderUI({
    kpi <- selected_kpi()
    if (is.null(kpi)) {
      kpi <- "Please select the field you are interested in"
    }
    
    div(
      style = "width: 100%; text-align: center; font-size: 20px; font-weight: bold;",
      kpi
    )
  })
  
  
  # Render both plots side by side
  output$kpi_dynamic_ui <- renderUI({
    kpi <- selected_kpi()
    if (is.null(kpi)) return(NULL)
    
    # Determine which plots to show
    if (kpi == "Item Fill Rate") {
      left_plot  <- plotOutput("ifr_boxplot")
      right_plot <- plotOutput("ifr_timeline")
    } else if (kpi == "On-Time Delivery") {
      left_plot  <- plotOutput("otd_boxplot")
      right_plot <- plotOutput("otd_timeline")
    } else if (kpi == "Return Rate") {
      left_plot  <- plotOutput("otd_boxplot") #Change
      right_plot <- plotOutput("otd_timeline") #Change
    }else {
      return(div("Unknown KPI"))
    }
    
    fluidRow(
      column(6, left_plot),
      column(6, right_plot)
    )
  })
  
  #Render Handlungsempfehlung
  output$kpi_result <- renderUI({
    kpi <- selected_kpi()
    vals <- kpi_vals()
    
    if (is.null(kpi) || is.null(vals)) return(NULL)
    
    proposal <- ""
    
    if (kpi == "Item Fill Rate" && !is.null(vals$ifr_value)) {
      x <- round(100 - vals$ifr_value, 1)
      proposal <- paste("Suggestion: Order", x, "% more units.")
    } else if (kpi == "On-Time Delivery" && !is.null(vals$delay)) {
      x <- round(vals$delay, 1)
      proposal <- paste("Suggestion: Order", x, "days earlier.")
    } else {
      return(NULL)  # No known KPI or data
    }
    
    div(
      id = "info-container",
      class = "info-box",
      style = paste(
        "margin-top: 20px; padding: 15px;",
        "border-radius: 8px; background-color: royalblue;",
        "color: white; text-align: center; font-size: 20px; font-weight: bold;"
      ),
      proposal
    )
  })
  
})