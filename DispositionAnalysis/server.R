# server.R

# 0) Pakete laden
library(shiny)
library(DT)        # für DataTable
library(ggplot2)   # für Plots
library(readxl)    # für read_excel()
library(dplyr)     # für filter, left_join, distinct
library(writexl)   # für Excel-Export

# 1) Alle Excel-Files einmalig einlesen (guess_max dämpft Typ-Warnings)
bom   <- read_excel("data/bom.xlsx",    guess_max = 10000)
MARA  <- read_excel("data/MARA.xlsx",   guess_max = 10000)
MARC  <- read_excel("data/MARC.xlsx",   guess_max = 10000)
MVER  <- read_excel("data/MVER.xlsx",   guess_max = 10000)

# Procurement
EBAN  <- read_excel("data/Procurement/EBAN.xlsx", guess_max = 10000)
EKBE  <- read_excel("data/Procurement/EKBE.xlsx", guess_max = 10000)
EKES  <- read_excel("data/Procurement/EKES.xlsx", guess_max = 10000)
EKET  <- read_excel("data/Procurement/EKET.xlsx", guess_max = 10000)
EKKO  <- read_excel("data/Procurement/EKKO.xlsx", guess_max = 10000)
EKPO  <- read_excel("data/Procurement/EKPO.xlsx", guess_max = 10000)

# Production
AFKO  <- read_excel("data/Production/AFKO.xlsx", guess_max = 10000)
AFPO  <- read_excel("data/Production/AFPO.xlsx", guess_max = 10000)
AFVC  <- read_excel("data/Production/AFVC.xlsx", guess_max = 10000)
AFVV  <- read_excel("data/Production/AFVV.xlsx", guess_max = 10000)

# 2) Bestellungen + Kopf + Lieferdatum zusammenführen
#    EKKO auf eindeutige Bestell-Köpfe reduzieren, EKET enthält das Lieferdatum-Feld EINDT
EKKO_unique <- EKKO %>%
  distinct(EBELN, .keep_all = TRUE)

orders <- EKPO %>%
  left_join(EKKO_unique, by = "EBELN") %>%
  left_join(
    EKET %>% select(EBELN, EBELP, EINDT),
    by = c("EBELN", "EBELP")
  ) %>%
  rename(Lieferdatum = EINDT) %>%
  mutate(
    # falls Lieferdatum als Zahl/Text gespeichert, in Date umwandeln
    Lieferdatum = as.Date(Lieferdatum),
    # hier musst du ggf. deine Durchlaufzeit-Spalte anpassen
    Durchlaufzeit = as.numeric(Durchlaufzeit)
  )

# 3) Shiny-Server-Logik
shinyServer(function(input, output, session) {
  
  # a) Dynamisches Befüllen des Werk-Dropdowns
  observe({
    werk_choices <- orders$WERKS %>% unique() %>% sort()
    updateSelectInput(session, "werk",
                      choices  = werk_choices,
                      selected = werk_choices[1])
  })
  
  # b) Reaktive Filterung nach ausgewähltem Werk & Datum
  df_filtered <- reactive({
    req(input$werk, input$zeitraum)
    orders %>%
      filter(
        WERKS        == input$werk,
        Lieferdatum >= input$zeitraum[1],
        Lieferdatum <= input$zeitraum[2]
      )
  })
  
  # c) Plot: Histogramm der Durchlaufzeiten
  output$durchlaufPlot <- renderPlot({
    df_filtered() %>%
      ggplot(aes(x = Durchlaufzeit)) +
      geom_histogram(binwidth = 1) +
      labs(
        title = "Verteilung der Durchlaufzeiten",
        x     = "Durchlaufzeit (Tage)",
        y     = "Anzahl Aufträge"
      ) +
      theme_minimal()
  })
  
  # d) Interaktive DataTable
  output$dtTable <- renderDataTable({
    df_filtered()
  }, options = list(pageLength = 10, autoWidth = TRUE))
  
  # e) Download-Button: Export der gefilterten Daten als Excel
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Prozessanalyse_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(df_filtered(), path = file)
    }
  )
})
