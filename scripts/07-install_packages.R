#### Preamble ####
# Purpose: Install packages used throughout the project
# Author: Lexi Knight
# Date: 30 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT


#### Workspace setup ####

# Install the necessary packages
install.packages(c(
  "arrow",      # For saving files as Parquet
  "dplyr",      # For data manipulation
  "forcats",    # For reordering factor levels
  "ggplot2",    # For data visualization
  "here",       # For working with relative paths
  "httr",       # For downloading data
  "kableExtra", # For rendering tables with extra features
  "openxlsx",   # For working with Excel files
  "partykit",   # For running the model
  "readr",      # For reading data
  "readxl",     # For reading Excel files
  "rpart",      # For running the model
  "stringr",    # For string manipulation
  "testthat",   # For testing data
  "tidyr",      # For tidying data
  "tidyverse",  # For data manipulation and visualization
  "tinytable"   # For rendering tables
))

