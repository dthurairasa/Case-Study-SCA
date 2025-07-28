# ui.R
library(shiny)
library(shinydashboard)
library(shinythemes)
library(colourpicker)

shinyUI(fluidPage(
  #  titlePanel("Prozessanalyse"),
  
  theme = shinytheme("flatly"),
  
  tags$head(
    tags$style(HTML("
      .top-bar {
        background-color: #f8f9fa;
        padding: 10px 20px;
        display: flex;
        justify-content: space-between;
        align-items: center;
        border-bottom: 1px solid #ddd;
      }
      .info-box {
        font-weight: bold;
        font-size: 16px;
      }
      .close-button {
        background-color: grey;
        color: white;
        border: none;
        padding: 6px 12px;
        font-size: 14px;
        cursor: pointer;
        border-radius: 4px;
      }
      .kpi-card {
        background-color: royalblue;
        border-radius: 8px;
        color: white;
        padding: 20px;
        margin: 10px;
        flex: 1 1 22%;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .kpi-number {
        font-size: 48px;
        font-weight: bold;
        margin: 10px 0;
      }
      .kpi-name {
        font-size: 20px;
      }
      .kpi-logo {
        font-size: 36px;
      }
      .kpi-container {
        display: flex;
        justify-content: space-around;
        flex-wrap: wrap;
        margin-top: 20px;
      }
    ")),
    tags$script(HTML("Shiny.addCustomMessageHandler('update-bar-color', function(col) { var el = document.getElementById('info-container'); if(el){ el.style.backgroundColor = col; } });"))
  ),
  
  # Top Bar
  div(
    style = "padding-top: 20px;",
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      div(
        style = "display: flex; gap: 100px; align-items: center;",
        selectInput("material", "Material:", choices = NULL, width = "180px"),
        span(
          HTML("Data Used:<br>"),
          textOutput("data_range", inline = TRUE)
        ),
        span(
          HTML("Datasets Used:<br>"),
          textOutput("data_count", inline = TRUE)
        ),
        uiOutput("all_data_checkbox")
      ),
      actionButton("close_app", "Close", class = "close-button")
    )
  ),
  
  tags$hr(style = "margin: 0; border-top: 1px solid #ddd;"),
  
  # KPI Cards Display (static placeholders for now)
  div(class = "kpi-container",
      style = "margin-top: 20px; padding: 15px; border: 1px solid #ddd; border-radius: 8px; display: flex; gap: 10px;",
      
      # Single button for IFR
      actionButton("kpi_ifr", label = div(
        class = "kpi-card",
        div(class = "kpi-logo", img(src = "IFR.png", height = 60)),
        div(class = "kpi-number", textOutput("kpi_ifr")),
        div(class = "kpi-name", "Item Fill Rate")
      ),
      style = "background: none; border: none; padding: 0; width: auto; text-align: left;"
      ),
      
      div(style = "width: 1px; background-color: #ddd; height: auto;"),
      
      # One button wrapping the last three KPIs
      actionButton("kpi_time", label = div(
        style = "display: flex; gap: 10px;",
        div(class = "kpi-card",
            div(class = "kpi-logo", img(src = "OTD.png", height = 60)),
            div(class = "kpi-number", textOutput("kpi_otdr")),
            div(class = "kpi-name", "On-Time Delivery Rate")
        ),
        div(class = "kpi-card",
            div(class = "kpi-logo", img(src = "OCT.png", height = 60)),
            div(class = "kpi-number", textOutput("kpi_oct")),
            div(class = "kpi-name", "Order Cycle Time")
        ),
        div(class = "kpi-card",
            div(class = "kpi-logo", img(src = "LTD.png", height = 60)),
            div(class = "kpi-number", textOutput("kpi_delay")),
            div(class = "kpi-name", "Mean Delay")
        )
      ),
      style = "background: none; border: none; padding: 0; width: auto; text-align: left; display: flex;"
      )
  ),
  
  div(id = "info-container",
      class = "info-box",
      style = paste(
        "margin-top: 20px; padding: 15px; border: 1px solid #ddd;",
        "border-radius: 8px; background-color: #cccccc;"
      ),
      uiOutput("kpi_info")
  ), 
  uiOutput("kpi_dynamic_ui")
  
))

