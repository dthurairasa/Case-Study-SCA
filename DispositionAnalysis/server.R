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
    
    list(
      ifr   = calculate_ifr (input$material, EKET, EKBE, EKPO, po_filter = po_keys),
      otdr  = calculate_otdr(input$material, EKET, EKES, EKPO, po_filter = po_keys),
      poct  = calculate_poct(input$material, EKPO, EKKO, EKBE, po_filter = po_keys),
      ltime = calculate_lead_time(input$material, EBAN, EKKO,       po_filter = po_keys)
    )
  })
  
  ## KPI-Cards (Text-Outputs für ui.R)
  output$kpi_ifr   <- renderText( sprintf("%.1f %%", kpi_vals()$ifr) )
  output$kpi_otdr  <- renderText( sprintf("%.1f %%", kpi_vals()$otdr * 100) )
  output$kpi_poct  <- renderText( sprintf("%.1f d",  kpi_vals()$poct) )
  output$kpi_lead  <- renderText( sprintf("%.1f d",  kpi_vals()$ltime) )
  ## ------------------------------------------
  ## 3) Extra-Information
  ## ------------------------------------------
  selected_kpi <- reactiveVal(NULL)
  
  observeEvent(input$kpi_ifr, {
    selected_kpi("Item Information")
  })
  observeEvent(input$kpi_time, {
    selected_kpi("Time Information")
  })
  
  output$kpi_info <- renderUI({
    text <- if (is.null(selected_kpi())) {
      "Please select the field you are interested in"
    } else {
      selected_kpi()
    }
    tags$div(style = "text-align: center;", text)
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