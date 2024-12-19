library(shiny)
library(DT)
library(lubridate)
library(shinycssloaders)
library(httr)
library(jsonlite)
library(dplyr)

ui <- tags$html(
  lang = "en",  # Set the language attribute here
  fluidPage(
    titlePanel(
      HTML('<span style="color: #002c3d;"><strong>WEDC, Loughborough University:</strong></span>
          <span style="color: #009BC9;">Illustrations and Graphics</span><br><br>')
    ),
    
    # CSS to set the background color and font size
    tags$head(
      tags$style(HTML("
        body {
          background-color: #FFFFFF;
          font-size: 16px;
        }
        h2, a {
          color: #6F3092;
        }
        a.hover-underline:hover {
          text-decoration: underline;
        }
      "))
    ),
    
    # Add a dropdown menu for selecting a collection name
    fluidRow(
      style = "margin-left: 20px; margin-right: 20px; margin-bottom: 10px;",
      column(
        width = 12,
        uiOutput("collectionDropdown") # Placeholder for the dropdown menu
      )
    ),
    
    # Show a table with a spinner
    fluidRow(
      style = "margin-left: 20px; margin-right: 20px;",
      withSpinner(
        dataTableOutput("articleTable"), # Table output
        type = 3, 
        color = "#009BC9", 
        color.background = "#FFFFFF"
      )
    )
  )
)
