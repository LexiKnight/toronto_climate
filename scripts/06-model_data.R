#### Preamble ####
# Purpose: Constructs a linear regression model for predicting likelihood of taking climate action based on age and education. 
# Author: Lexi Knight
# Date: 30 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete 01-download_data.R and 03-clean_data.R in scripts folder in order to access data.


#### Workspace setup ####

library(tidyverse)
library(readr)
library(here)
library(caret)
library(arrow)

#### Read data ####
# Load your analysis data
climate_data <- read_csv(here::here("data/02-analysis_data/twenty_eighteen_individual_analysis_data.csv"))

# Clean the data
climate_data_cleaned <- climate_data %>%
  filter(
    !is.na(likelihood_home_improvement),  # Ensure no missing values in the response variable
    !is.na(age),                          # Ensure no missing values in the predictor variable age
    !is.na(highest_level_educ)             # Ensure no missing values in the predictor variable education
  ) %>%
  mutate(
    # Convert likelihood of home improvement to an ordered factor
    likelihood_home_improvement = factor(
      likelihood_home_improvement,
      levels = c(
        "very unlikely",
        "somewhat unlikely",
        "somewhat likely",
        "very likely",
        "already doing this or have done this"
      ),
      ordered = TRUE  # Indicate that the factor is ordinal
    ),
    likelihood_numeric = as.numeric(likelihood_home_improvement),  # Convert ordinal factor to numeric for regression
    age = as.numeric(age),  # Ensure age is treated as a numeric variable
    highest_level_educ = factor(highest_level_educ, levels = c(
      "high school or less",
      "some community college / trade school",
      "completed community college / trade school",
      "some university",
      "completed undergraduate degree",
      "postgraduate / professional school",
      "prefer not to answer"
    )) %>% droplevels()  # Drop unused levels
  )

# Check if the factor levels are valid
print(levels(climate_data_cleaned$highest_level_educ))  # Display levels to ensure they are correct

# Fit the linear regression model
if (nlevels(climate_data_cleaned$highest_level_educ) > 1) {
  climate_model <- lm(likelihood_numeric ~ age + highest_level_educ, data = climate_data_cleaned)
  
  # Save the model to an RDS file for future use
  saveRDS(climate_model, here::here("models/climate_model_home_improvement.rds"))
  
  # Save the cleaned dataset to a Parquet file for further analysis
  write_parquet(climate_data_cleaned, here::here("data/analysis_data/climate_action_home_improvement_model_data.parquet"))
  
  # Output model summary for diagnostics
  print(summary(climate_model))
} else {
  stop("The variable 'highest_level_educ' has less than two levels. Please check the data.")
}