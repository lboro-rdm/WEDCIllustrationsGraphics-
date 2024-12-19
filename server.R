server <- function(input, output, session) {
  
  # Reactive function to fetch and filter the data based on the selected collection
  filteredArticles <- reactive({
    csv_file <- "figshare_articles_with_collections.csv"
    if (file.exists(csv_file)) {
      combined_df <- read.csv(csv_file, stringsAsFactors = FALSE)
      
      if (!is.null(combined_df) && nrow(combined_df) > 0) {
        if (!is.null(input$selected_collection) && input$selected_collection != "") {
          combined_df <- combined_df[combined_df$collection_name == input$selected_collection, ]
        }
        
        # Sort by year and then by title
        combined_df <- combined_df %>%
          arrange(year, title) %>%  # Sort alphabetically by year and title
          mutate(formatted_text = paste("WEDC, Loughborough University. (", year, "). ", 
                                        title, ". Loughborough University. Figure. ", 
                                        "<a href='", doi, "' target='_blank'>", doi, "</a>.", sep = "")) %>%
          select(formatted_text) # Select only the formatted text column
      } else {
        combined_df <- data.frame(formatted_text = "No data available")
      }
    } else {
      combined_df <- data.frame(formatted_text = "CSV file not found")
    }
    combined_df
  })
  
  # Render the dropdown menu for collection names
  output$collectionDropdown <- renderUI({
    csv_file <- "figshare_articles_with_collections.csv"
    if (file.exists(csv_file)) {
      combined_df <- read.csv(csv_file, stringsAsFactors = FALSE)
      if (!is.null(combined_df) && nrow(combined_df) > 0) {
        collection_names <- unique(combined_df$collection_name)
        collection_names <- sort(collection_names)  # Sort collection names alphabetically
        selectInput(
          inputId = "selected_collection",
          label = "Select Collection",
          choices = c("", collection_names),
          selected = ""
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
  escape = FALSE,    # Allow HTML for clickable DOIs
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
