library(httr)
library(jsonlite)
library(dplyr)

# Get article IDs -----------------------------------------------------

collection_ids <- read.csv("collection_ids.csv", stringsAsFactors = FALSE)$collection_id

# Initialize a data frame to store results
article_details <- data.frame(
  collection_name = character(),
  article_id = character(),
  title = character(),
  doi = character(),
  stringsAsFactors = FALSE
)

# Figshare API base URL
base_url <- "https://api.figshare.com/v2"

# Function to fetch collection details and articles
fetch_articles_from_collection <- function(collection_id) {
  # Fetch collection details (name)
  collection_url <- paste0(base_url, "/collections/", collection_id)
  collection_response <- GET(collection_url)
  
  if (status_code(collection_response) != 200) {
    message("Failed to fetch collection details for ID: ", collection_id)
    return(NULL)
  }
  
  collection_name <- fromJSON(content(collection_response, as = "text"))$title
  
  # Fetch articles in the collection
  articles_url <- paste0(base_url, "/collections/", collection_id, "/articles?page_size=1000")
  articles_response <- GET(articles_url)
  
  if (status_code(articles_response) != 200) {
    message("Failed to fetch articles for collection ID: ", collection_id)
    return(NULL)
  }
  
  articles <- fromJSON(content(articles_response, as = "text"))
  
  # Format articles into a data frame
  data.frame(
    collection_name = collection_name,
    article_id = as.character(articles$id),  # Ensure article_id is a character
    title = articles$title,
    doi = articles$doi,
    stringsAsFactors = FALSE
  )
}

# Loop through each collection ID and fetch article details
for (collection_id in collection_ids) {
  message("Fetching articles for collection ID: ", collection_id)
  collection_articles <- fetch_articles_from_collection(collection_id)
  if (!is.null(collection_articles)) {
    article_details <- bind_rows(article_details, collection_articles)
  }
}

# Create a column with DOI as clickable links
article_details <- article_details %>%
  mutate(doi = ifelse(!is.na(doi), paste0("https://doi.org/", doi), NA))


# Get Tag details ---------------------------------------------------------

# Function to fetch article details using article ID
fetch_article_details <- function(article_id) {
  # Figshare API endpoint for article details
  article_details_url <- paste0(base_url, "/articles/", article_id)
  article_details_response <- GET(article_details_url)
  
  # Check if the request was successful
  if (status_code(article_details_response) != 200) {
    message("Failed to fetch details for article ID: ", article_id)
    return(data.frame(
      article_id = as.character(article_id),
      tags = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  # Parse the response
  article_details <- fromJSON(content(article_details_response, as = "text"))
  
  # Extract tags
  tags <- if (!is.null(article_details$tags)) {
    if ("Line drawings" %in% article_details$tags) {
      "Line drawings"
    } else {
      "Technical illustration"
    }
  } else {
    "Technical illustration"
  }
  
  # Return a data frame with the article ID and tags
  data.frame(
    article_id = as.character(article_id),
    tags = tags,
    stringsAsFactors = FALSE
  )
}

# Initialize a counter
total_articles <- nrow(article_details)
message("Total articles to process: ", total_articles)

# Fetch tags for all articles with a progress counter
article_tags <- lapply(seq_along(article_details$article_id), function(i) {
  message("Processing article ", i, " of ", total_articles, " (ID: ", article_details$article_id[i], ")")
  fetch_article_details(article_details$article_id[i])
})

# Combine results into a single data frame
tags_data <- bind_rows(article_tags)

# Merge the tags data back into the original article details
article_details <- left_join(article_details, tags_data, by = "article_id")

article_details <- article_details %>%
  distinct(collection_name, article_id, .keep_all = TRUE)


# Write to CSV ------------------------------------------------------------

# Save the updated data frame with tags to a CSV file
write.csv(article_details, "articles_details.csv", row.names = FALSE)
