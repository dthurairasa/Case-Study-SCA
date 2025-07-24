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
  ## 0) Hilfs-Reaktivitäten (Werk & Material)
  ## ------------------------------------------
  
  # Material-Liste für Drop-down
  observe({
    req(orders)
    mats <- sort(unique(orders$MATNR))
    updateSelectInput(session, "material",
                      choices  = mats,
                      selected = first(mats))
  })
  
  ## ------------------------------------------
  ## 1) Daten nach Werk & Zeitraum filtern
  ## ------------------------------------------
  df_filtered <- reactive({
    req(input$zeitraum)
    orders %>%
      filter(
        Lieferdatum >= input$zeitraum[1],
        Lieferdatum <= input$zeitraum[2]
      )
  })
  
  ## ------------------------------------------
  ## 2) KPI-Berechnung (pro Material)
  ## ------------------------------------------
  kpi_vals <- reactive({
    req(input$material)
    
    list(
      # Reihenfolge der Argumente anpassen!
      ifr   = calculate_ifr (input$material, EKET, EKBE, EKPO),     # %-Wert 0-100
      otdr  = calculate_otdr(input$material, EKET, EKES, EKPO),     # Rate   0-1
      poct  = calculate_poct(input$material, EKPO, EKKO, EKBE),     # Tage
      ltime = calculate_lead_time(input$material, EBAN, EKKO)       # Tage
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
  
  
})