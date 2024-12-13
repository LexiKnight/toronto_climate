#### Preamble ####
# Purpose: Tests the structure and validity of the cleaned climate change data.
# Author: Lexi Knight
# Date: 2 December 2024 
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: Complete both 02-download_data.R and 03-clean_data.R


#### Workspace setup ####
library(tidyverse)     # reads csv file
library(testthat)      # write and execute tests


### Load data outside of test block ###
analysis_data <- read_csv("data/02-analysis_data/twenty_eighteen_individual_analysis_data.csv", col_names = TRUE)

## Test Data ###### Test Data for Climate Change Perspectives ####

test_that("Climate Change Perspectives Data Cleaning Tests", {
  
  # Test if the dataset has the expected number of entries
  expect_equal(nrow(analysis_data), 404)  # Modify the expected value if needed
  
  # Test if 'age' is numeric
  expect_true(all(is.numeric(analysis_data$age)))
  
  # Test if 'age' has realistic values (e.g., ages between 18 and 120)
  expect_true(all(analysis_data$age >= 18 & analysis_data$age <= 120))
  
  # Test if 'extent_consider_informed' contains expected categories
  expect_true(all(analysis_data$extent_consider_informed %in% c('Extremely informed', 'Very informed', 'Not very informed', 'Not at all informed')))
  
  # Test if 'likelihood_home_improvement' contains expected categories
  expect_true(all(analysis_data$likelihood_home_improvement %in% c('Already doing this or have done this', 'Very likely', 'Somewhat likely', 'Somewhat unlikely', 'Very unlikely')))
  
  # Test if 'likelihood_reduce_hydro' contains expected categories
  expect_true(all(analysis_data$likelihood_reduce_hydro %in% c('Already doing this or have done this', 'Very likely', 'Somewhat likely', 'Somewhat unlikely', 'Very unlikely')))
  
  # Test if 'likelihood_minimize_car' contains expected categories
  expect_true(all(analysis_data$likelihood_minimize_car %in% c('Already doing this or have done this', 'Very likely', 'Somewhat likely', 'Somewhat unlikely', 'Very unlikely')))
  
  # Test if 'likelihood_vehicle_electric' contains expected categories
  expect_true(all(analysis_data$likelihood_vehicle_electric %in% c('Already doing this or have done this', 'Very likely', 'Somewhat likely', 'Somewhat unlikely', 'Very unlikely')))
  
  # Test if 'likelihood_protein_alternative' contains expected categories
  expect_true(all(analysis_data$likelihood_protein_alternative %in% c('Already doing this or have done this', 'Very likely', 'Somewhat likely', 'Somewhat unlikely', 'Very unlikely')))
  
  # Test if 'likelihood_reduce_waste' contains expected categories
  expect_true(all(analysis_data$likelihood_reduce_waste %in% c('Already doing this or have done this', 'Very likely', 'Somewhat likely', 'Somewhat unlikely', 'Very unlikely')))
  
  # Test if 'delivery_method_toronto.ca_website' contains expected categories
  expect_true(all(analysis_data$delivery_method_toronto.ca_website %in% c('no', 'toronto.ca website')))
  
  # Test if 'delivery_method_events' contains expected categories
  expect_true(all(analysis_data$delivery_method_events %in% c('no', 'events')))
  
  # Test if 'delivery_method_twitter' contains expected categories
  expect_true(all(analysis_data$delivery_method_twitter %in% c('no', 'twitter')))
  
  # Test if 'delivery_method_facebook' contains expected categories
  expect_true(all(analysis_data$delivery_method_facebook %in% c('no', 'facebook')))
  
  # Test if 'delivery_method_instagram' contains expected categories
  expect_true(all(analysis_data$delivery_method_instagram %in% c('no', 'instagram')))
  
  # Test if 'delivery_method_enewsletter_email' contains expected categories
  expect_true(all(analysis_data$delivery_method_enewsletter_email %in% c('no', 'enewsletter / email')))
  
  # Test if 'delivery_method_councillor_communication' contains expected categories
  expect_true(all(analysis_data$delivery_method_councillor_communication %in% c('no', 'councillor communication')))
  
  # Test if 'delivery_method_advertising_campaigns' contains expected categories
  expect_true(all(analysis_data$delivery_method_advertising_campaigns %in% c('no', 'advertising campaigns')))
  
  # Test if 'delivery_method_brochures_pamphlets' contains expected categories
  expect_true(all(analysis_data$delivery_method_brochures_pamphlets %in% c('no', 'brochures / pamphlets')))
  
  # Test if 'delivery_method_other' contains expected categories
  expect_true(all(analysis_data$delivery_method_other %in% c('no', 'other')))
                          
  # Test if 'delivery_method_not_interested_receiving' contains expected categories
  expect_true(all(analysis_data$delivery_method_not_interested_receiving %in% c('no', 'not interested')))
                          
  # Test if 'highest_level_educ' contains valid education categories
  expect_true(all(analysis_data$highest_level_educ %in% c('High school or less',
                                                          'Some community college, vocation, trade school',
                                                          'Completed community college, vocation, trade school',
                                                          'Some university',
                                                          'Completed undergraduate degree',
                                                          'Post graduate/professional school',
                                                          'Prefer not to answer')))
  
  # Test for missing values in critical columns
  expect_true(all(!is.na(analysis_data$extent_consider_informed)))
  expect_true(all(!is.na(analysis_data$likelihood_home_improvement)))
  expect_true(all(!is.na(analysis_data$delivery_method_toronto.ca_website)))
  expect_true(all(!is.na(analysis_data$highest_level_educ)))
  
  # Test if all 'likelihood' columns are within valid range (5 categories)
  likelihood_columns <- grep("^likelihood_", names(analysis_data), value = TRUE)
  for (col in likelihood_columns) {
    expect_true(all(analysis_data[[col]] %in% c('Already doing this or have done this', 'Very likely', 'Somewhat likely', 'Somewhat unlikely', 'Very unlikely')))
  }
  
  # Check if all 'delivery_method' columns are alphabetical 
  delivery_columns <- grep("^delivery_method_", names(analysis_data), value = TRUE)
  for (col in delivery_columns) {
    expect_true(all(analysis_data[[col]] %in% c(0, 1)))
  }
})
