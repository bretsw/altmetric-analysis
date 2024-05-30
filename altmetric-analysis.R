################################################################################

library(tidyverse)
library(readxl)
source("local-functions.R")

################################################################################

#test_url <- "https://api.altmetric.com/v1/doi/10.1038/480426a"
#test_doi <- "10.1038/480426a"
#fetch_altmetric_data(test_doi) %>% glimpse()

################################################################################

## Read in article list
article_df <-
  readxl::read_xlsx("sample-articles.xlsx",
                    sheet = "Article List")

## Define the DOIs
doi_list <- article_df$DOI

################################################################################

## Fetch Altmetric data for each DOI and extract mentions
altmetric_results_list <-
  map(doi_list, fetch_altmetric_data)

altmetric_results_df <-
  do.call(bind_rows, altmetric_results_list) %>%
  group_by(pick(-authors)) %>%
  summarize(authors = paste(authors, collapse = ", "),
            .groups = 'drop') %>%
  select(doi, title, authors, everything()) %>%
  arrange(desc(score), doi) %>%
  mutate(across(score:included_in_citeulike, ~ coalesce(., 0)))

#View(altmetric_results_df)

################################################################################

merged_results_df <-
  article_df %>%
  left_join(select(altmetric_results_df, -title, -authors, -journal),
            by = c("DOI" = "doi")
  )

#View(merged_results_df)

################################################################################

write_csv(merged_results_df, "altmetric-results.csv")
