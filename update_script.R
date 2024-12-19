library(httr)
library(jsonlite)
library(dplyr)

# Read collection IDs from CSV
collection_ids <- read.csv("collection_ids.csv", stringsAsFactors = FALSE)$collection_id

# Initialize a data frame to store results
article_details <- data.frame(
  collection_name = character(),
  title = character(),
  year = integer(),
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
    title = articles$title,
    year = as.integer(format(as.Date(articles$published_date), "%Y")),
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

# Write the results to a CSV file
write.csv(article_details, "figshare_articles_with_collections.csv", row.names = FALSE)

message("Article details saved to figshare_articles_with_collections.csv")
