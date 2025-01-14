library(httr)
library(jsonlite)
library(dplyr)
library(magick)
library(tools)

# Get article IDs -----------------------------------------------------

start_csv <- Sys.time() 

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

article_details <- article_details %>%
  mutate(doi = ifelse(
    !is.na(doi), 
    paste0("https://doi.org/", sub("\\.v[0-9]+$", "", doi)), 
    NA
  ))

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
      "Technical illustration and graphics"
    }
  } else {
    "Technical illustration and graphics"
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

write.csv(article_details, "articles_details.csv", row.names = FALSE)

end_csv <- Sys.time()

total_csv_time <- end_csv - start_csv


# Get image files ---------------------------------------------------------

# Paths for log and output directories
processed_log <- "processed_ids.csv"
thumbnails_dir <- "www/thumbnails"  # Save thumbnails in www/thumbnails

# Load previously processed IDs
if (file.exists(processed_log)) {
  processed_ids <- read.csv(processed_log, stringsAsFactors = FALSE)$article_id
} else {
  processed_ids <- character(0)
}

# Filter out processed IDs
unique_article_ids <- setdiff(unique(article_details$article_id), processed_ids)

start_images <- Sys.time()

# Loop through unprocessed article IDs
for (i in seq_along(unique_article_ids)) {
  article_id <- unique_article_ids[i]
  message("Fetching file IDs for article ID: ", article_id, " (", i, " of ", length(unique_article_ids), ")")
  
  # Fetch file IDs for the article
  files_url <- paste0(base_url, "/articles/", article_id, "/files")
  files_response <- GET(files_url)
  
  if (status_code(files_response) != 200) {
    message("Failed to fetch file info for article ID: ", article_id)
    next
  }
  
  files_info <- fromJSON(content(files_response, as = "text"))
  
  # Check if there are files available for the article
  if (nrow(files_info) > 0) {
    # Loop through each file and download it
    for (j in seq_len(nrow(files_info))) {
      file <- files_info[j, ]
      if (!is.null(file$download_url)) {  # Ensure 'download_url' exists
        download_url <- file$download_url
        
        message("Downloading file for article ID: ", article_id, " (File Name: ", file$name, ")")
        
        # Download the file directly into memory for thumbnail creation
        temp_file <- tempfile(fileext = ".tmp")
        download.file(download_url, temp_file, mode = "wb")
        
        # Resize the image to thumbnail using magick
        thumbnail_file <- file.path(thumbnails_dir, paste0(article_id, "_thumbnail.jpg"))
        image <- image_read(temp_file)  # Read the downloaded image from memory
        image_resized <- image_scale(image, "150x150!")  # Resize to 150x150 pixels
        image_write(image_resized, thumbnail_file, format = "jpg")  # Save the thumbnail
        message("Created thumbnail for article ID: ", article_id, " (Saved as: ", thumbnail_file, ")")
        
        # Remove the temporary file to avoid saving the original image
        unlink(temp_file)
      } else {
        message("No download URL found for article ID: ", article_id)
      }
    }
    # Log the successfully processed article ID
    write.csv(data.frame(article_id = article_id), processed_log, row.names = FALSE, append = TRUE)
  } else {
    message("No files available for article ID: ", article_id)
  }
}



end_images <- Sys.time()

total_time_images <- end_images - start_images

print(total_time_images)
print(total_csv_time)
# 9 minutes to get the csv, 2 hours to get images - 2024-12-20, PM
# 8 minutes to get the csv, 44 minutes to get images - 2024-12-21 AM, Saturday
# 9 mins to get csv, 24 mins to get images - 2025-01-02 AM
# 9 mins to get csv, 34 mins to get images - 2025-01-08

# Match Thumbnails to Articles ------------------------------------------------

thumbnails_dir <- "www/thumbnails"

# Get a list of all thumbnails in the directory
thumbnail_files <- list.files(thumbnails_dir, full.names = TRUE)

# Extract article IDs from the thumbnail filenames
thumbnail_data <- data.frame(
  article_id = gsub("_thumbnail\\..*$", "", basename(thumbnail_files)),  # Extract article_id from filename
  thumbnail_file = basename(thumbnail_files),  # Store the file name
  stringsAsFactors = FALSE
)

# Join the thumbnail data with the article details
article_details <- article_details %>%
  left_join(thumbnail_data, by = "article_id")

# Write the updated article details with thumbnails to CSV
write.csv(article_details, "articles_details_with_thumbnails.csv", row.names = FALSE)

message("Updated articles_details.csv with thumbnail file names.")
