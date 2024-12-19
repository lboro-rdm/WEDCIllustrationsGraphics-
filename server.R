server <- function(input, output, session) {
  
  # Reactive function to fetch and filter the data based on the selected collection
  filteredArticles <- reactive({
    csv_file <- "figshare_articles_with_collections.csv"
    if (file.exists(csv_file)) {
      combined_df <- read.csv(csv_file, stringsAsFactors = FALSE)
      
      if (!is.null(combined_df) && nrow(combined_df) > 0) {
        if (!is.null(input$selected_collection) && input$selected_collection != "") {
          if (input$selected_collection == "All") {
            # If "All" is selected, no filtering is applied
            filtered_df <- combined_df
          } else {
            # Filter by the selected collection
            filtered_df <- combined_df[combined_df$collection_name == input$selected_collection, ]
          }
        } else {
          filtered_df <- combined_df
        }
        
        # Sort by year and title, then remove duplicates
        filtered_df <- filtered_df %>%
          arrange(year, title) %>%  # Sort alphabetically by year and title
          distinct(year, title, .keep_all = TRUE) %>%  # Remove duplicates based on year and title
          mutate(formatted_text = paste("<span style='color: #002c3d;'>WEDC, Loughborough University. (", year, "). ", 
                                        "<a href='", doi, "' target='_blank' style='text-decoration: underline; color: #002c3d;'>", title, "</a>. Loughborough University. Figure.</span>", 
                                        sep = "")) %>%
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
    csv_file <- "figshare_articles_with_collections.csv"
    if (file.exists(csv_file)) {
      combined_df <- read.csv(csv_file, stringsAsFactors = FALSE)
      if (!is.null(combined_df) && nrow(combined_df) > 0) {
        collection_names <- unique(combined_df$collection_name)
        collection_names <- sort(collection_names)  # Sort collection names alphabetically
        # Add "All" option
        collection_choices <- c("All", collection_names)
        selectInput(
          inputId = "selected_collection",
          label = "Select Collection",
          choices = collection_choices,
          selected = "All"  # Set "All" as the default selected option
        )
      }
    } else {
      "No collections available."
    }
  })
  
  # Render the filtered article details as a table
  output$articleTable <- renderDataTable({
    filteredArticles()
  }, 
  escape = FALSE,    # Allow HTML for clickable titles
  rownames = FALSE,  # Remove row numbering
  options = list(
    paging = FALSE,
    ordering = FALSE,  # Disable column ordering since there's only one column
    searching = TRUE,   # Enable table search
    headerCallback = JS(
      "function( thead, data, start, end, display ) {",
      "  $(thead).hide();",  # Hide the table header
      "}"
    )
  ))
}
