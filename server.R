server <- function(input, output, session) {
  
  # Reactive function to fetch and filter the data based on the selected collection
  filteredArticles <- reactive({
    csv_file <- "articles_details.csv"
    if (file.exists(csv_file)) {
      combined_df <- read.csv(csv_file, stringsAsFactors = FALSE)
      
      if (!is.null(combined_df) && nrow(combined_df) > 0) {
        # Filter by the selected collection
        if (!is.null(input$selected_collection) && input$selected_collection != "") {
          if (input$selected_collection == "All") {
            filtered_df <- combined_df
          } else {
            filtered_df <- combined_df[combined_df$collection_name == input$selected_collection, ]
          }
        } else {
          filtered_df <- combined_df
        }
        
        # Filter by the selected drawing type
        if (!is.null(input$drawing_type) && input$drawing_type != "" && input$drawing_type != "All") {
          filtered_df <- filtered_df[filtered_df$tags == input$drawing_type, ]
        }
        
        filtered_df <- filtered_df %>%
          arrange(title) %>%  
          distinct(doi, .keep_all = TRUE) %>%  
          mutate(formatted_text = paste(
            "<a href='", doi, "' target='_blank' style='text-decoration: underline; color: #002c3d;'>", title, "</a>")) %>%
          select(formatted_text) # Select only the formatted text column
      } else {
        filtered_df <- data.frame(formatted_text = "No data available")
      }
    } else {
      filtered_df <- data.frame(formatted_text = "CSV file not found")
    }
    filtered_df
  })
  
  # Render the dropdown menu for collection names
  output$collectionDropdown <- renderUI({
    csv_file <- "articles_details.csv"
    if (file.exists(csv_file)) {
      combined_df <- read.csv(csv_file, stringsAsFactors = FALSE)
      if (!is.null(combined_df) && nrow(combined_df) > 0) {
        collection_names <- unique(combined_df$collection_name)
        collection_names <- sort(collection_names)
        collection_choices <- c("All", collection_names)
        selectInput(
          inputId = "selected_collection",
          label = "Select Collection",
          choices = collection_choices,
          selected = "All"
        )
      } else {
        "No collections available."
      }
    } else {
      "CSV file not found."
    }
  })
  
  # Render the dropdown menu for drawing types
  output$drawingTypeDropdown <- renderUI({
    csv_file <- "articles_details.csv"
    if (file.exists(csv_file)) {
      combined_df <- read.csv(csv_file, stringsAsFactors = FALSE)
      if (!is.null(combined_df) && nrow(combined_df) > 0) {
        drawing_types <- unique(combined_df$tags)
        drawing_types <- sort(drawing_types)
        drawing_choices <- c("All", drawing_types)  # Include "All" option
        selectInput(
          inputId = "drawing_type",
          label = "Select Drawing Type",
          choices = drawing_choices,
          selected = "All"  # Set "All" as the default selected option
        )
      } else {
        "No drawing types available."
      }
    } else {
      "CSV file not found."
    }
  })
  
  # Render the filtered article details as a table
  output$articleTable <- renderDataTable({
    filteredArticles()
  }, 
  escape = FALSE,
  rownames = FALSE,
  options = list(
    paging = FALSE,
    ordering = FALSE,
    searching = TRUE,
    headerCallback = JS(
      "function( thead, data, start, end, display ) {",
      "  $(thead).hide();",
      "}"
    )
  ))
}
