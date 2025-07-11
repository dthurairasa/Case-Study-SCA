# ui.R
library(shiny)
library(shinythemes)
library(colourpicker)

shinyUI(fluidPage(
#  titlePanel("Prozessanalyse"),

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
        display: flex;
        justify-content: space-between; /* spreads items evenly */
        width: 90%;
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
    "))
    ),
    
    # Top Bar
    div(class = "top-bar",
        div(class = "info-box",
            span(HTML("Material:<br>*Material Number*</b>")),
            span(HTML("Lead Time:<br>*Lead Time*</b>")),
            span(HTML("Data Used:<br>from 01.01.2023 to 31.05.2025</b>"))
        ),
        actionButton("close_app", "Close", class = "close-button")
    ),
    
    
    # KPI Cards Display (static placeholders for now)
    div(class = "kpi-container",
        div(class = "kpi-card",
            div(class = "kpi-logo", img(src = "IFR.png", height = "60px")),
            div(class = "kpi-number", "93,4%"),
            div(class = "kpi-name", "Item Fill Rate")
        ),
        div(class = "kpi-card",
            div(class = "kpi-logo", img(src = "OTD.png", height = "60px")),
            div(class = "kpi-number", "85,8%"),
            div(class = "kpi-name", "On Time Delivery Rate")
        ),
        div(class = "kpi-card",
            div(class = "kpi-logo", img(src = "LTD.png", height = "60px")),
            div(class = "kpi-number", "1,3d"),
            div(class = "kpi-name", "Lead Time")
        ),
        div(class = "kpi-card",
            div(class = "kpi-logo", img(src = "OCT.png", height = "60px")),
            div(class = "kpi-number", "145d"),
            div(class = "kpi-name", "Order Cycle Time")
        )
    )
))