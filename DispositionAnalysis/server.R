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
      
      
      otdr  = calculate_otdr(input$material, EKET, EKES, EKPO, po_filter = po_keys),
      poct  = calculate_poct(input$material, EKPO, EKKO, EKBE, po_filter = po_keys),
      ltime = calculate_lead_time(input$material, EBAN, EKKO,       po_filter = po_keys)
    )
  })
  
  ## KPI-Cards (Text-Outputs für ui.R)
  output$kpi_ifr <- renderText(sprintf("%.1f %%", kpi_vals()$ifr_value))
  output$kpi_otdr  <- renderText( sprintf("%.1f %%", kpi_vals()$otdr * 100) )
  output$kpi_poct  <- renderText( sprintf("%.1f d",  kpi_vals()$poct) )
  output$kpi_lead  <- renderText( sprintf("%.1f d",  kpi_vals()$ltime) )
  
  
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
  
  output$ifr_timeline <- renderPlot({
    df <- kpi_vals()$ifr_time
    ggplot(df, aes(x = Lieferdatum, y = ifr_line)) +
      geom_line() + geom_point() +
      labs(title = "IFR-Zeitreihe", y = "IFR %", x = "Lieferdatum") +
      theme_minimal()
  })
  
  ## -------------------------------------------------
  ## 4) Zusätzliche Ampel-Info (bei Klick auf KPI-Karte)
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
  
  observeEvent(input$kpi_ifr, {
    info  <- get_color_info(kpi_vals()$ifr_value, avg_ifr())
    selected_kpi ("Item Fill Rate")
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
  
  ## ------------------------------------------
  ## 3) Histogramm der Durchlaufzeiten
  ## ------------------------------------------
  output$durchlaufPlot <- renderPlot({
    ggplot(df_filtered(), aes(x = Durchlaufzeit)) +
      geom_histogram(binwidth = 1) +
      labs(title = "Verteilung der Durchlaufzeiten",
           x = "Durchlaufzeit (Tage)",
           y = "Anzahl Aufträge") +
      theme_minimal()
  })
  
  ## ------------------------------------------
  ## 4) Tabelle
  ## ------------------------------------------
  output$dtTable <- renderDataTable(
    df_filtered(),
    options = list(pageLength = 10, autoWidth = TRUE)
  )
  
  ## ------------------------------------------
  ## 5) Download
  ## ------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() paste0("Prozessanalyse_", Sys.Date(), ".xlsx"),
    content  = function(file) writexl::write_xlsx(df_filtered(), path = file)
  )
})