#### Preamble ####
# Purpose: Exploring 2018 individual data as well as 2018 and 2021 summary data. 
# Author: Lexi Knight
# Date: 27 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete 01-download_data.R and 03-clean_data.R in scripts folder in order to access raw data.


#### Workspace setup ####

# install libraries 
# install.packages(c("tidyverse", "dplyr", "ggplot2", "here", "stringr", "knitr", "arrow"))

# Load necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(here)
library(stringr)
library(knitr)
library(arrow)

# Read in the data 
individual_18 <- read_parquet(here("data/02-analysis_data/twenty_eighteen_individual_analysis_data.parquet"))

# Create a histogram to show the distribution of age
age_individual_plot <- ggplot(individual_18, aes(x = age_18)) + 
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(
    title = "Age Distribution of Survey Respondents (2018)",
    x = "Age",
    y = "Frequency"
  ) + 
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(0, max(individual_18$age_18, na.rm = TRUE), by = 20),  # Set ticks every 20 years
    limits = c(0, max(individual_18$age_18, na.rm = TRUE))               # Optional: adjust x-axis limits if needed
  )

# Print the plot
print(age_individual_plot)


# Create a histogram to show the distribution of highest level of education attained
# Define the order of education categories
education_order <- c(
  "High school or less",
  "Some community college, vocational, trade school",
  "Completed community college, vocational, trade school",
  "Some university",
  "Completed undergraduate degree",
  "Post graduate/professional school",
  "Prefer not to answer"
)

# Convert the education column to a factor with the desired levels
individual_18$highest_level_educ_18 <- factor(individual_18$highest_level_educ_18, levels = education_order)

# Create a histogram to show the distribution of highest level of education attained
educ_individual_plot <- ggplot(individual_18, aes(x = highest_level_educ_18)) +
  geom_bar(fill = "forestgreen", color = "black", alpha = 0.7) +  # Use geom_bar for categorical data
  labs(
    title = "Education Distribution of Survey Respondents (2018)",
    x = "Education Level",
    y = "Frequency"
  ) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
# tilt x-axis labels for better readability

# Print the plot
print(educ_individual_plot)



# Clean the 'highest_level_educ_18' column (if it is a character type)
individual_18$highest_level_educ_18 <- trimws(individual_18$highest_level_educ_18)  # Remove any leading/trailing spaces
individual_18$highest_level_educ_18[individual_18$highest_level_educ_18 == ""] <- NA  # Convert empty strings to NA

# Define the correct order of education categories
education_order <- c(
  "High school or less",
  "Some community college, vocational, trade school",
  "Completed community college, vocational, trade school",
  "Some university",
  "Completed undergraduate degree",
  "Post graduate/professional school",
  "Prefer not to answer"
)

# Convert the column to a factor with the correct levels
individual_18$highest_level_educ_18 <- factor(individual_18$highest_level_educ_18, levels = education_order)

# Check for missing or invalid data again
table(individual_18$highest_level_educ_18, useNA = "ifany")




