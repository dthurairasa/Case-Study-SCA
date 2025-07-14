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
  # Drop-down „Werk“ füllen, sobald orders da ist
  observe({
    req(orders)
    werke <- sort(unique(orders$WERKS))
    
    updateSelectInput(session, "werk",
                      choices  = werke,
                      selected = first(werke))
  })
  
  # Material-Liste für zweites Drop-down (falls noch nicht im UI)
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
    req(input$werk, input$zeitraum)
    orders %>%
      filter(
        WERKS        == input$werk,
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