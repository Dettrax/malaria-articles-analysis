# Malaria Articles Analysis

This project aims to gather information from a large library of articles related to malaria, organize the data, perform clustering to identify main topics across the articles, and analyze the significant words associated with each cluster.

## Overview

The project consists of several steps:

1. **Web Scraping**: Data is collected from a website hosting malaria-related articles, including publication date, authors, abstracts, and keywords.

2. **Data Preprocessing**: Abstracts are cleaned up by removing punctuation and common stopwords.

3. **Clustering**: K-means clustering is applied to group the articles based on their content.

4. **Analysis**: The significant words for each cluster are identified using TF-IDF scores.

## Getting Started

To run the code, follow these steps:

1. Clone the repository to your local machine:

2. Install the required R packages listed in the `requirements.txt` file.

3. Run the main R script `malaria_analysis.R` to perform the analysis.

## Dependencies

The following R packages are required to run the code:

- `rvest`: For web scraping.
- `tm`: For text mining and preprocessing.
- `dplyr`, `tidyverse`: For data manipulation and visualization.
- `stringr`: For string manipulation.

Install these packages using the following command:
install.packages(c("rvest", "tm", "dplyr", "tidyverse", "stringr"))


## Usage

Modify the parameters in the main script `malaria_analysis.R` as needed, such as the total number of iterations for scraping or the optimal number of clusters for k-means.


## Acknowledgments

- This project was inspired by the need to analyze and summarize information from a vast collection of malaria-related articles.
- Special thanks to the developers of the R packages used in this project for their contributions to the R community.
