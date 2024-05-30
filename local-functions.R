################################################################################

library(tidyverse)
library(openssl)
library(httr)
library(jsonlite)

################################################################################

# Define your Altmetric API key and secret
api_key <- Sys.getenv('ALTMETRIC_API_KEY')
api_secret <- Sys.getenv('ALTMETRIC_API_SECRET')

# Create a SHA-256 digest of the API secret
digest <- openssl::sha256(charToRaw(api_secret))
digest_hex <- paste(as.character(digest), collapse = "")
rm(api_secret)

################################################################################

# Function to fetch Altmetric data for a single DOI

fetch_altmetric_data <-
  function(doi, key = api_key) {

    # Construct the URL for the Altmetric API request
    base_url <- "https://api.altmetric.com/v1/doi/"
    url <- paste0(base_url, doi)

    # Make the GET request to the Altmetric API with the API key and digest in the headers
    response <-
      httr::GET(url,
                add_headers(
                  Authorization = paste("Bearer", api_key),
                  `x-altmetric-signature` = digest_hex
                ))

    #httr::stop_for_status(response, "authenticate with Altmetric API key")

    # Check if the request was successful
    if (status_code(response) == 200) {
      # Parse the response content as JSON
      altmetric_data <-
        jsonlite::fromJSON(content(response, as = "text", encoding = "UTF-8"))
      altmetric_df <-
        tibble(doi = altmetric_data$doi,
               title = altmetric_data$title,
               authors = altmetric_data$authors,
               url = altmetric_data$url,
               details_url = altmetric_data$details_url,
               journal = altmetric_data$journal,
               type = altmetric_data$type,
               subjects = altmetric_data$subjects,
               open_access = altmetric_data$is_oa,
               score = altmetric_data$score,
               cited_by_facebook = altmetric_data$cited_by_fbwalls_count,
               cited_by_feeds = altmetric_data$cited_by_feeds_count,
               cited_by_googleplus = altmetric_data$cited_by_gplus_count,
               cited_by_posts = altmetric_data$cited_by_posts_count,
               cited_by_tweets = altmetric_data$cited_by_tweeters_count,
               cited_by_wikipedia = altmetric_data$cited_by_wikipedia_count,
               cited_by_accounts = altmetric_data$cited_by_accounts_count,
               cited_by_msm = altmetric_data$cited_by_msm_count,
               included_in_mendeley = as.integer(altmetric_data$readers$mendeley),
               included_in_connotea = as.integer(altmetric_data$readers$connotea),
               included_in_citeulike = as.integer(altmetric_data$readers$citeulike)
        )
      return(altmetric_df)

    } else {
      # Print the error message
      warning(paste("Failed to retrieve data for DOI:",
                    doi,
                    "- Status code:",
                    status_code(response)))
      altmetric_df <-
        tibble(doi = doi,
               title = NA,
               authors = NA,
               url = NA,
               details_url = NA,
               journal = NA,
               type = NA,
               subjects = NA,
               open_access = NA,
               score = 0,
               cited_by_facebook = 0,
               cited_by_feeds = 0,
               cited_by_googleplus = 0,
               cited_by_posts = 0,
               cited_by_tweets = 0,
               cited_by_wikipedia = 0,
               cited_by_accounts = 0,
               cited_by_msm = 0,
               included_in_mendeley = 0,
               included_in_connotea = 0,
               included_in_citeulike = 0
        )
      return(altmetric_df)
    }

  }
