#### Preamble ####
# Purpose: Install packages used throughout the project
# Author: Lexi Knight
# Date: 30 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT


#### Workspace setup ####

# Install the necessary packages
install.packages(c(
  "readr",      # For reading data
  "dplyr",      # For data manipulation
  "openxlsx",   # For working with Excel files
  "readxl",     # For reading Excel files
  "arrow",      # For saving files as Parquet
  "tidyverse",  # For data manipulation and visualization
  "tidyr",      # For tidying data
  "stringr",    # For string manipulation
  "tinytable",  # For rendering tables
  "here",       # For working with relative paths
  "knitr",      # For dynamic report generation
  "forcats",    # For reordering factor levels
  "kableExtra", # For rendering tables with extra features
  "broom",      # For tidying model output
  "testthat"    # For testing data
))


install.packages("caret")