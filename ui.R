library(shiny)
library(DT)
library(lubridate)
library(shinycssloaders)
library(httr)
library(jsonlite)
library(dplyr)

ui <- tags$html(
  lang = "en",
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
    
    # Sidebar layout
    sidebarLayout(
      sidebarPanel(
        style = "margin-bottom: 10px;",
        uiOutput("collectionDropdown"),
        uiOutput("drawingTypeDropdown"),  # New dropdown for Drawing Type
        textInput("search_title", "Search Title:", ""),  # Search box for titles
        p(),
        p("These figures were prepared by WEDC, School of Architecture, Building and Civil Engineering, Loughborough University.")
      ),
      mainPanel(
        withSpinner(
          uiOutput("articleGrid"), # Table output
          type = 3, 
          color = "#009BC9", 
          color.background = "#FFFFFF"
        )
      )
    )
  )
)
