#### Preamble ####
# Purpose: Constructs a linear regression model for predicting likelihood of taking climate action based on age and education. 
# Author: Lexi Knight
# Date: 30 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete 01-download_data.R and 03-clean_data.R in scripts folder in order to access data.


#### Workspace setup ####

# Install required packages
install.packages("tidyverse")  # For data manipulation
install.packages("broom")  # Optional, for tidying model output


library(tidyverse)
library(brms)
library(arrow)
library(here)
library(broom)

#### Read data ####
# Load your analysis data
individual_18 <- read_parquet(here("data/02-analysis_data/twenty_eighteen_individual_analysis_data.parquet"))

# Convert the 'highest_level_educ' variable to a factor
climate_data$highest_level_educ <- factor(climate_data$highest_level_educ)

# Now check the levels again
levels(climate_data$highest_level_educ)


# Convert the ordinal likelihood variables to numeric values
climate_data <- climate_data %>%
  mutate(across(starts_with("likelihood"),
                ~ as.numeric(factor(., levels = c("very_unlikely", "somewhat_unlikely", 
                                                  "somewhat_likely", "very_likely", "already_done")))))

# Fit a linear regression model
linear_model <- lm(likelihood_home_improvement ~ age + highest_level_educ, data = climate_data)

# Summarize the model
summary(linear_model)
