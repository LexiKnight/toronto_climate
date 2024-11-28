#### Preamble ####
# Purpose: Exploring 2018 individual data as well as 2018 and 2021 summary data. 
# Author: Lexi Knight
# Date: 27 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete 01-download_data.R and 03-clean_data.R in scripts folder in order to access raw data.


#### Workspace setup ####

# install libraries 
# install.packages(c("tidyverse", "dplyr", "ggplot2", "here", "stringr", "knitr"))

# Load necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(here)
library(stringr)
library(knitr)

# Read in the data from your CSV file
individual_18 <- read_csv(here("data/02-analysis_data/twenty_eighteen_individual_analysis_data.parquet"))

# Create a histogram to show the distribution of age
age_individual_plot <- ggplot(data, aes(x = age_18)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(
    title = "Age Distribution of Survey Respondents (2018)",
    x = "Age",
    y = "Frequency"
  ) +
  theme_minimal()

# Display the plot
print(age_individual_plot)

# calculate summary statistics 
mean_age <- mean(data$age_18, na.rm = TRUE)
median_age <- median(data$age_18, na.rm = TRUE)
min_age <- min(data$age_18, na.rm = TRUE)
max_age <- max(data$age_18, na.rm = TRUE)
# R does not have built in function thus needs to be defined
sd_age <- sd(data$age_18, na.rm = TRUE)
mode_age <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}
mode_value <- mode_age(data$age_18)

# Create a data frame with the summary statistics
summary_stats_age_18 <- data.frame(
  Statistic = c("Mean Age", "Median Age", "Mode Age", "Standard Deviation", "Minimum Age", "Maximum Age"),
  Value = c(mean_age, median_age, mode_value, sd_age, min_age, max_age)
)

# Use kable to generate a nice table
kable(summary_stats_age_18, 
      caption = "Summary Statistics for Age of Survey Respondents", 
      col.names = c("Statistic", "Value"),
      align = c("l", "c"))



