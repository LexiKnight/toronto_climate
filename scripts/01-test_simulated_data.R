#### Preamble ####
# Purpose: Tests the structure and validity of the simulated climate change perspectives data.
# Date: 26 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete the 00-simulate_data.R script. 

#### Workspace setup ####
# Load required packages
library(tidyverse)  # For data manipulation and visualization
library(testthat)   # For writing and running tests
library(arrow)      # For reading parquet files
library(here)       # for file path management


#### Test data ####

test_that("Simulated Data Tests", {
  
  # Load simulated data
  simulated_data <- read_parquet(file = here::here("data/00-simulated_data/simulated_climate_change_data.parquet"))
  
  # Test if the dataset has 404 entries
  expect_equal(nrow(simulated_data), 404)
  
  # Test if 'age' is within the range of 18 to 100
  expect_true(all(simulated_data$age >= 18 & simulated_data$age <= 100))
  
  # Test if 'education' contains the correct categories
  expect_true(all(simulated_data$education %in% c("high school or less",
                                                  "some community college / trade school",
                                                  "completed community college / trade school", 
                                                  "some university",
                                                  "completed undergraduate degree",
                                                  "post graduate / professional school",
                                                  "prefer not to answer")))
  
  # Test if 'informed' contains the correct categories
  expect_true(all(simulated_data$informed %in% c("extremely informed",
                                                 "very informed",
                                                 "not very informed",
                                                 "not at all informed")))
  
  # Test if 'likelihood' contains the correct categories
  expect_true(all(simulated_data$likelihood %in% c("already doing this or have done this",
                                                   "very likely",
                                                   "somewhat likely",
                                                   "somewhat unlikely",
                                                   "very unlikely")))
  
  # Test if 'communication' contains the correct categories
  expect_true(all(simulated_data$communication %in% c("Toronto.ca website", 
                                                      "city of toronto events",
                                                      "twitter",
                                                      "facebook",
                                                      "instagram",
                                                      "city of toronto enewsletter / email",
                                                      "councillor communications",
                                                      "advertising campaigns",
                                                      "brochures / pamphlets",
                                                      "other",
                                                      "not interested")))
  
  # Test if 'age' is numeric
  expect_true(is.numeric(simulated_data$age))
  
  # Test if 'education', 'informed', 'likelihood', and 'communication' are characters
  expect_true(is.character(simulated_data$education))
  expect_true(is.character(simulated_data$informed))
  expect_true(is.character(simulated_data$likelihood))
  expect_true(is.character(simulated_data$communication))
  
  # Check the data types again for confirmation
  print(class(simulated_data$age))
  print(class(simulated_data$education))
  print(class(simulated_data$informed))
  print(class(simulated_data$likelihood))
  print(class(simulated_data$communication))
  
  #### Additional Tests ####
  
  # Test if there are any missing values in critical columns
  expect_true(all(!is.na(simulated_data$age)))
  expect_true(all(!is.na(simulated_data$education)))
  expect_true(all(!is.na(simulated_data$informed)))
  expect_true(all(!is.na(simulated_data$likelihood)))
  expect_true(all(!is.na(simulated_data$communication)))
  
  # Check for duplicates and show the duplicated rows
  duplicate_rows <- simulated_data[duplicated(simulated_data), ]
  print(duplicate_rows)  # Display duplicate rows for investigation
  
  # If duplicates are found, remove them
  simulated_data <- unique(simulated_data)  # Removing duplicates
  
  # Test if there are any duplicate rows after removing
  expect_equal(nrow(simulated_data), nrow(unique(simulated_data)))
  
  # Test if 'education' contains only expected categories
  expect_true(all(simulated_data$education %in% c("high school or less",
                                                  "some community college / trade school",
                                                  "completed community college / trade school", 
                                                  "some university",
                                                  "completed undergraduate degree",
                                                  "post graduate / professional school",
                                                  "prefer not to answer")))
  
  # Test if the 'education' categories have reasonable distribution (i.e., no overly dominant categories)
  education_counts <- table(simulated_data$education)
  expect_true(all(education_counts > 20))  # Ensure each category has at least 20 entries
  
  # Test if 'age' distribution is reasonable (e.g., no ages below 18 or above 100)
  expect_true(all(simulated_data$age >= 18 & simulated_data$age <= 100))
  
})
