# Imagine you're gathering information from a huge library of articles all about malaria. First, you grab data from a website where these articles are hosted. You collect details like the publication date, who wrote the articles, what they're about (the abstract), and some important keywords.
# 
# Once you have all this information, you start organizing it. You clean up the abstracts, removing unnecessary stuff like punctuation and common words that don't tell us much. Then, you turn all these abstracts into a big table, kind of like a spreadsheet, so you can easily see what's in each one.
# 
# Now, here comes the interesting part: you want to find out what the main topics are across all these articles. So, you use a special technique called clustering. It's like sorting the articles into groups based on what they talk about. For example, some articles might be about mosquito control methods, while others might focus on the genetics of malaria parasites.
# 
# After the clustering, you dig deeper into each group to see what words pop up the most. These words give you a clue about what's important in each topic. You create these cool charts showing which words are most significant for each cluster of articles.
# 
# In the end, you've gone from a massive pile of malaria-related articles to a neat summary of the main topics and key words discussed in those articles. It's like putting together a puzzle to reveal the big picture of what's being researched and talked about in the world of malaria.




# Load necessary packages for web scraping and data manipulation
library(rvest)     # For web scraping
library(httr)      # For making HTTP requests
library(xml2)      # For parsing XML and HTML content
library(dplyr)     # For data manipulation
library(magrittr)  # For using the pipe operator
library(tidyverse) # For data wrangling and visualization

# Function to extract publication date from a journal link
get_date <- function(journal_link) {
  journal_page <- read_html(journal_link)
  journal_date <- journal_page %>% html_nodes(".c-article-identifiers__item time") %>%
    html_text(trim = TRUE) %>% paste(collapse = ",")
  return(journal_date)
}

# Function to extract corresponding author from a journal link
get_corauthor <- function(journal_link) {
  journal_page <- read_html(journal_link)
  corauth <- journal_page %>% html_nodes("#corresponding-author-list") %>%
    html_text(trim = TRUE) %>% paste(collapse = ",")
  return(corauth)
}

# Function to extract abstract from a journal link
get_abstract <- function(journal_link) {
  journal_page <- read_html(journal_link)
  abst <- journal_page %>% html_nodes("#Abs1-content p") %>%
    html_text(trim = TRUE) %>% paste(collapse = ",")
  return(abst)
}

# Function to extract keywords from a journal link
get_keywords <- function(journal_link) {
  journal_page <- read_html(journal_link)
  keywrd <- journal_page %>% html_nodes(".c-article-subject-list__subject a") %>%
    html_text(trim = TRUE) %>% paste(collapse = ",")
  return(keywrd)
}

# Initialize an empty list to store data frames
df_list <- list()

# Total number of iterations for scraping
total_iterations <- 162

# Iterate over pages to scrape data
for (page_result in 1:total_iterations) {
  url <- paste0("https://malariajournal.biomedcentral.com/articles?page=", page_result)
  
  # Try to read the HTML content of the page
  journal <- tryCatch({
    read_html(url)
  }, error = function(e) {
    # Handle errors related to SSL/TLS connection and general errors
    if(grepl("SSL/TLS connection timeout", conditionMessage(e))) {
      cat("SSL/TLS connection timeout for URL:", url, "\n")
      return(NULL)
    } else if(grepl("SSL/TLS", conditionMessage(e))) {
      cat("SSL/TLS error for URL:", url, "\n")
      return(NULL)
    } else {
      cat("Error occurred while reading the HTML content for URL:", url, "\n")
      return(NULL)
    }
  })
  
  # Skip iteration if the page couldn't be read
  if (is.null(journal)) {
    next
  }
  
  # Extract journal links from the page
  journal_links <- journal %>% html_nodes(".c-listing__title a") %>%
    html_attr("href") %>%
    paste0("https://malariajournal.biomedcentral.com", .)
  
  # Process each journal link if available
  if (length(journal_links) > 0) {
    Title <- journal %>% html_nodes(".c-listing__title a") %>% html_text(trim = TRUE)
    Author <- journal %>% html_nodes(".c-listing__authors-list") %>% html_text(trim = TRUE)
    PublishDate <- sapply(journal_links, get_date, USE.NAMES = FALSE)
    CorrAuthor <- sapply(journal_links, get_corauthor, USE.NAMES = FALSE)
    Abstract <- sapply(journal_links, get_abstract, USE.NAMES = FALSE)
    Keywords <- sapply(journal_links, get_keywords, USE.NAMES = FALSE)
    
    # Create a temporary data frame for the current page and add it to the list
    temp_df <- data.frame(Title, Author, PublishDate, CorrAuthor, Abstract, Keywords, stringsAsFactors = FALSE)
    df_list <- c(df_list, list(temp_df))
  }
}

# Combine data frames in the list into a single data frame
df <- do.call(rbind, df_list)


# Load necessary packages for text mining and preprocessing
library(tm)      # For text mining
library(dplyr)   # For data manipulation
library(stringr) # For string manipulation

# Function to preprocess abstracts
preprocess_abstract <- function(abstract) {
  if (!is.na(abstract) && !is.null(abstract)) {
    abstract <- tolower(abstract)
    abstract <- removePunctuation(abstract)
    tokens <- unlist(str_split(abstract, "\\s+"))
    tokens <- tokens[!tokens %in% stopwords("en")]
    return(paste(tokens, collapse = " "))
  } else {
    return('')
  }
}

# Preprocess abstracts in the dataframe
df$PreprocessedAbstract <- sapply(df$Abstract, preprocess_abstract)

# Filter out rows with empty abstracts
df <- df[df$PreprocessedAbstract != '', ]

# Create document-term matrix
corpus <- Corpus(VectorSource(df$PreprocessedAbstract))
dtm <- DocumentTermMatrix(corpus)

# Apply TF-IDF transformation
dtm_tfidf <- weightTfIdf(dtm)
tfidf_matrix <- as.matrix(dtm_tfidf)

# Set the optimal number of clusters
k_optimal <- 37

# Perform k-means clustering
kmeans_model <- kmeans(tfidf_matrix, centers = k_optimal, iter.max = 100)

# Get cluster centroids and terms
centroids <- kmeans_model$centers
terms <- colnames(tfidf_matrix)

# Get the most important words per cluster
important_words_per_cluster <- lapply(1:k_optimal, function(i) {
  important_word_indices <- order(centroids[i, ], decreasing = TRUE)[1:20]
  important_words <- terms[important_word_indices]
  return(important_words)
})

# Calculate TF-IDF scores for each cluster
tfidf_scores <- lapply(1:k_optimal, function(i) {
  cluster_indices <- kmeans_model$cluster == i
  cluster_tfidf_scores <- colMeans(tfidf_matrix[cluster_indices, , drop = FALSE])
  names(cluster_tfidf_scores) <- terms
  return(cluster_tfidf_scores)
})

# Create a dataframe to store cluster information
result_df <- data.frame(Cluster_ID = 0:(k_optimal-1))

# Initialize lists to store word lists and importance factors
word_lists <- list()
importance_factors_list <- list()

# Populate lists with words and importance factors
for (i in 1:k_optimal) {
  important_words <- important_words_per_cluster[[i]]
  importance_factors <- tfidf_scores[[i]]
  
  words_str <- paste("'", paste(important_words, collapse = "', '"), "'", sep = "")
  importance_factors_str <- paste(importance_factors, collapse = ", ")
  
  word_lists[[i]] <- words_str
  importance_factors_list[[i]] <- importance_factors_str
}

# Add word lists and importance factors to the dataframe
result_df$Words <- word_lists
result_df$Importance_Factor <- importance_factors_list

# Function to retrieve importance factors for a given cluster and words
get_importance_factors <- function(result_df, cluster_id, words) {
  cluster_row <- result_df[result_df$Cluster_ID == cluster_id, ]
  if (nrow(cluster_row) == 0) {
    print("Invalid cluster ID.")
    return(NULL)
  }
  importance_factors_str <- as.character(cluster_row$Importance_Factor)
  importance_factors <- as.numeric(unlist(strsplit(importance_factors_str, ", ")))
  importance_factors <- importance_factors[1:length(words)]
  names(importance_factors) <- words
  return(importance_factors)
}

# Function to plot importance factors for given words and cluster
plot_importance_factor <- function(words, importance_factors, xlabel, title) {
  barplot(importance_factors, names.arg = words, col = "skyblue", 
          xlab = xlabel, ylab = "Importance Factor", main = title,
          las = 2, cex.names = 0.8, ylim = c(0, max(importance_factors) * 1.2),
          mar = c(5, 5, 4, 2) + 0.1)
}

# Example usage of get_importance_factors and plot_importance_factor functions

# Cluster 1
cluster_id <- 1
words <- c('resist', 'insecticid', 'pyrethroid', 'vector', 'mosquito', 'deltamethrin', 'control', 'malaria', 'bioassay', 'net')
importance_factors <- get_importance_factors(result_df, cluster_id, words)
xlabel <- 'Words'
title <- 'Importance Factors of Words Associated with Mosquito Resistance and Insecticide Usage in Malaria Control Efforts'
plot_importance_factor(words, importance_factors, xlabel, title)

# Cluster 2
cluster_id <- 2
words <- c( 'net', 'bed', 'household', 'llin', 'mosquito', 'insecticid', 'itn', 'malaria', 'insecticidetr', 'prevent')
importance_factors <- get_importance_factors(result_df, cluster_id, words)
xlabel <- 'Words'
title <- 'Importance Factors of Words Associated with Mosquito Nets and Insecticide-Treated Nets in Malaria Prevention'
plot_importance_factor(words, importance_factors, xlabel, title)

# Cluster 3
cluster_id <- 3
words_cluster_3 <- c('g6pd', 'primaquin', 'malaria', 'genotyp', 'enzym', 'mutat', 'pq', 'elimin')
importance_factors_cluster_3 <- get_importance_factors(result_df, cluster_id, words_cluster_3)
xlabel <- 'Words'
title <- 'Importance Factors of Words Associated with G6PD Deficiency and Primaquine in Malaria Elimination'
plot_importance_factor(words_cluster_3, importance_factors_cluster_3, xlabel, title)

# Cluster 11
cluster_id <- 11
words_cluster_11 <- c('malaria', 'model', 'risk', 'transmiss', 'spatial', 'climat', 'incid', 'predict', 'preval', 'rainfal')
importance_factors_cluster_11 <- get_importance_factors(result_df, cluster_id, words_cluster_11)
xlabel <- 'Words'
title <- 'Importance Factors of Words Associated with Malaria Risk Modeling and Transmission Prediction'
plot_importance_factor(words_cluster_11, importance_factors_cluster_11, xlabel, title)

# Cluster 13
cluster_id <- 13
words_cluster_13 <- c('malaria', 'preval', 'children', 'associ', 'household', 'infect', 'risk', 'health', 'area', 'survey')
importance_factors_cluster_13 <- get_importance_factors(result_df, cluster_id, words_cluster_13)
xlabel <- 'Words'
title <- 'Importance Factors of Words Associated with Malaria Prevalence in Children and Household Surveys'
plot_importance_factor(words_cluster_13, importance_factors_cluster_13, xlabel, title)
