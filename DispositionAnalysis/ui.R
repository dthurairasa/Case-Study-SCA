# ui.R
library(shiny)

shinyUI(fluidPage(
  titlePanel("Prozessanalyse"),
  
  sidebarLayout(
    sidebarPanel(
      # Hier später Dropdowns, Filters etc.
      selectInput("werk", "Werk:", choices = NULL),
      dateRangeInput("zeitraum", "Zeitraum:", start = Sys.Date()-30, end = Sys.Date())
    ),
    mainPanel(
      # Platz für Plots und Tabelle
      plotOutput("durchlaufPlot"),
      DT::dataTableOutput("dtTable")
    )
  )
))