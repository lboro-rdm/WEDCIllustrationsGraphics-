server <- function(input, output, session) {
  
  filteredArticles <- reactive({
    csv_file <- "articles_details_with_thumbnails.csv"
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
        
        # Filter by the search query
        if (!is.null(input$search_title) && input$search_title != "") {
          search_query <- tolower(input$search_title)
          filtered_df <- filtered_df[grepl(search_query, tolower(filtered_df$title)), ]
        }
        
        # Generate paths for thumbnails
        filtered_df <- filtered_df %>%
          arrange(title) %>%
          distinct(doi, .keep_all = TRUE) %>%
          mutate(
            thumbnail_path = paste0("thumbnails/", article_id, "_thumbnail.jpg"),  # Construct path to thumbnail
            formatted_html = paste0(
              "<div style='text-align: center;'>",
              "<img src='", thumbnail_path, "' alt='Thumbnail for article ", article_id, "' style='max-width: 150px; max-height: 150px; margin: 5px;' />",
              "<div style='margin-top: 5px; font-size: 14px;'>",
              "<a href='", doi, "' target='_blank' style='text-decoration: underline; color: #002c3d;'>", title, "</a>",
              "</div>",
              "</div>"
            )
          )
      } else {
        filtered_df <- data.frame(formatted_html = "No data available")
      }
    } else {
      filtered_df <- data.frame(formatted_html = "CSV file not found")
    }
    filtered_df
  })
  
  # Render the filtered article details as a dynamic table/grid
  output$articleGrid <- renderUI({
    filtered_df <- filteredArticles()
    if (!is.null(filtered_df) && nrow(filtered_df) > 0 && "formatted_html" %in% names(filtered_df)) {
      div(
        style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(150px, 1fr)); gap: 10px;",
        HTML(paste(filtered_df$formatted_html, collapse = ""))
      )
    } else {
      div("No articles to display.")
    }
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
    csv_file <- "articles_details_with_thumbnails.csv"
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
}
