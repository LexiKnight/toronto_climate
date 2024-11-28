#### Preamble ####
# Purpose: Exploring 2018 individual data as well as 2018 and 2021 summary data. 
# Author: Lexi Knight
# Date: 27 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete 01-download_data.R and 03-clean_data.R in scripts folder in order to access raw data.


#### Workspace setup ####

# install libraries 
# install.packages(c("tidyverse", "dplyr", "ggplot2", "here", "stringr", "knitr", "arrow", "tidyr"))
# install.packages("forcats")

# Load necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(here)
library(stringr)
library(knitr)
library(arrow)
library(tidyr)
library(forcats) # for reordering factor levels

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
# Convert the column to a factor
individual_18_raw$highest_level_educ_18 <- factor(individual_18_raw$highest_level_educ_18, 
                                                  levels = c("High school or less", 
                                                             "Some community college, vocational, trade school",
                                                             "Completed community college, vocational, trade school", 
                                                             "Some university",
                                                             "Completed undergraduate degree", 
                                                             "Post graduate/professional school",
                                                             "Prefer not to answer"))

# Create the plot with adjustments
educ_individual_plot <- ggplot(individual_18_raw, aes(x = highest_level_educ_18, fill = highest_level_educ_18)) +
  geom_bar(color = "black", alpha = 0.7) + 
  labs(
    title = "Education Distribution of Survey Respondents (2018)",
    x = "Education Level",
    y = "Frequency"
  ) + 
  theme_minimal() +
  scale_x_discrete(
    breaks = levels(individual_18_raw$highest_level_educ_18), # Set breaks according to the factor levels
    labels = str_wrap(levels(individual_18_raw$highest_level_educ_18), width = 20)  # Wrap x-axis labels to 20 characters
  ) +
  scale_y_continuous(
    breaks = seq(0, max(table(individual_18_raw$highest_level_educ_18)), by = 25)  # Add y-axis ticks every 25
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels by 45 degrees
    plot.margin = margin(10, 10, 20, 10)  # Increase the margins for better spacing
  ) +
  scale_fill_manual(values = c("High school or less" = "red", 
                               "Some community college, vocational, trade school" = "orange", 
                               "Completed community college, vocational, trade school" = "yellow", 
                               "Some university" = "lightgreen", 
                               "Completed undergraduate degree" = "green", 
                               "Post graduate/professional school" = "forestgreen", 
                               "Prefer not to answer" = "gray")) +
  guides(fill = guide_legend(title = NULL))  # Hide the legend title

# Display the plot
print(educ_individual_plot)







# Create a histogram to show the distribution of extent informed
# Convert the 'extent_consider_informed_18' column to a factor with specified levels in the desired order
individual_18_raw$extent_consider_informed_18 <- factor(individual_18_raw$extent_consider_informed_18,
                                                        levels = c("Extremely informed", 
                                                                   "Very informed", 
                                                                   "Not very informed", 
                                                                   "Not at all informed"))

# Create the plot with adjusted y-axis range
informed_plot <- ggplot(individual_18_raw, aes(x = extent_consider_informed_18, fill = extent_consider_informed_18)) +
  geom_bar(color = "black", alpha = 0.7) + 
  labs(
    title = "Distribution of Self-Reported Climate Change Knowledge (2018)",
    x = "Extent of Being Informed about Climate Change",
    y = "Frequency"
  ) + 
  theme_minimal() +
  scale_x_discrete(
    breaks = levels(individual_18_raw$extent_consider_informed_18), # Set breaks according to the factor levels
    labels = str_wrap(levels(individual_18_raw$extent_consider_informed_18), width = 20)  # Wrap x-axis labels to 20 characters
  ) +
  scale_y_continuous(
    limits = c(0, 250),  # Set y-axis range from 0 to 250
    breaks = seq(0, 250, by = 25)  # Add y-axis ticks every 25
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels by 45 degrees
    plot.margin = margin(10, 10, 20, 10)  # Increase the margins for better spacing
  ) +
  scale_fill_manual(values = c("Extremely informed" = "forestgreen", 
                               "Very informed" = "green", 
                               "Not very informed" = "orange", 
                               "Not at all informed" = "red")) +
  guides(fill = guide_legend(title = NULL))  # Hide the legend title

# Display the plot
print(informed_plot)







# Create a figure for likelihood of taking actions to address climate change
# Create a tidy data frame for easier plotting
actions_data <- individual_18_raw %>%
  select(likelihood_action_home_improvement_18, 
         likelihood_action_reduce_hydro_18, 
         likelihood_action_minimize_car_18, 
         likelihood_action_vehicle_electric_18, 
         likelihood_action_protein_alternative_18, 
         likelihood_action_reduce_waste_18, 
         likelihood_action_green_product_18, 
         likelihood_action_short_distance_18, 
         likelihood_action_sort_waste_18) %>%
  pivot_longer(cols = everything(), names_to = "action", values_to = "likelihood") %>%
  # Recode the action column to readable labels
  mutate(action = recode(action, 
                         "likelihood_action_home_improvement_18" = "Home Improvement",
                         "likelihood_action_reduce_hydro_18" = "Reduce Hydro Usage",
                         "likelihood_action_minimize_car_18" = "Minimize Car Use",
                         "likelihood_action_vehicle_electric_18" = "Electric/Hybrid Vehicle",
                         "likelihood_action_protein_alternative_18" = "Meat Alternatives",
                         "likelihood_action_reduce_waste_18" = "Reduce Waste",
                         "likelihood_action_green_product_18" = "Purchase Green Products",
                         "likelihood_action_short_distance_18" = "Walk/Cycle Short Distance",
                         "likelihood_action_sort_waste_18" = "Sort Waste Correctly"
  ))

# Plot stacked bar chart
ggplot(actions_data, aes(x = action, fill = likelihood)) +
  geom_bar(position = "fill") +  # Positioning bars to stack
  labs(title = "Likelihood of Taking Climate Change Actions",
       x = "Actions",
       y = "Proportion",
       fill = "Likelihood") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Already doing this or have done this" = "green",
                               "Very likely" = "lightgreen",
                               "Somewhat likely" = "yellow",
                               "Somewhat unlikely" = "orange",
                               "Very unlikely" = "red"))




# Create a figure illustrating reasons why people are not taking specific action to mitigate climate change
# Reshape data to long format (example code assumes data is in 'actions_data')

# Assuming your data is in a dataframe called 'actions_data'

# List of actions
actions <- c("home_improvement", "reduce_hydro", "minimize_car", "vehicle_electric",
             "protein_alternative", "reduce_waste", "green_product",
             "short_distance", "sort_waste")

# List of reasons
reasons <- c("confusing", "individual_difference", "ineffective", "costly",
             "unavailable", "inconvenient", "uninterested", "other")

# Reshape your data from wide to long format
actions_long <- actions_data %>%
  pivot_longer(cols = starts_with("unlikelihood_action"),
               names_to = c("Action", "Reason"),
               names_pattern = "unlikelihood_action_(.*)_(.*)_18",
               values_to = "Response") %>%
  filter(Response == "yes")  # Only keep "Yes" responses

# Create a grouped bar chart
ggplot(actions_long, aes(x = Action, fill = Reason)) +
  geom_bar(position = "dodge") +  # Group by reason
  coord_flip() +  # Horizontal bars for better readability
  labs(title = "Reasons for Not Taking Climate Change Actions",
       x = "Action",
       y = "Frequency of 'Yes' Responses",
       fill = "Reason") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust readability
        axis.text.y = element_text(size = 8))  # Adjust font size




