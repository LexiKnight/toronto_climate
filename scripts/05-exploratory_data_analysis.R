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
# install.packages("forcats","RColorBrewer")

# Load necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(here)
library(stringr) # was this used?
library(knitr)
library(arrow)
library(tidyr)
library(forcats) # for reordering factor levels


# Read in the data 
individual_18 <- read_parquet(here("data/02-analysis_data/twenty_eighteen_individual_analysis_data.parquet"))

## Age ##
# Calculate mean and median of age
mean_age <- mean(individual_18$age, na.rm = TRUE)
median_age <- median(individual_18$age, na.rm = TRUE)
# Create a histogram to show the distribution of age with mean and median lines
age_individual_plot <- ggplot(individual_18, aes(x = age)) + 
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) + 
  geom_vline(aes(xintercept = mean_age), color = "blue", linetype = "dashed", size = 1, 
             show.legend = TRUE) +  # Mean line
  geom_vline(aes(xintercept = median_age), color = "forestgreen", linetype = "dotted", size = 1, 
             show.legend = TRUE) +  # Median line
  labs(
    title = "Age Distribution of Survey Respondents (2018)",
    x = "Age",
    y = "Frequency",
    caption = "Blue dashed line = Mean | Green dotted line = Median"  # Legend explanation
  ) + 
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(15, max(individual_18$age, na.rm = TRUE), by = 15),  # Set ticks every 20 years starting at 15
    limits = c(15, max(individual_18$age, na.rm = TRUE))               # Adjust x-axis limits to start at 15
  )
# Print the plot
print(age_individual_plot)

# Save the plot to data/03-figures_data
ggsave(
  filename = here("data/03-figures_data", "age_individual_plot.png"),  # Specify the correct path and filename
  plot = age_individual_plot,
  width = 8, height = 6
)



## Education ##
# Create a histogram to show the distribution of highest level of education attained
# Convert the column to a factor
individual_18$highest_level_educ <- factor(individual_18$highest_level_educ, 
                                                  levels = c("High school or less", 
                                                             "Some community college, vocational, trade school",
                                                             "Completed community college, vocational, trade school", 
                                                             "Some university",
                                                             "Completed undergraduate degree", 
                                                             "Post graduate/professional school",
                                                             "Prefer not to answer"))
# Create the plot with adjustments
educ_individual_plot <- ggplot(individual_18, aes(x = highest_level_educ, fill = highest_level_educ)) +
  geom_bar(color = "black", alpha = 0.7) + 
  labs(
    title = "Education Distribution of Survey Respondents (2018)",
    x = "Education Level",
    y = "Frequency"
  ) + 
  theme_minimal() +
  scale_x_discrete(
    breaks = levels(individual_18$highest_level_educ), # Set breaks according to the factor levels
    labels = str_wrap(levels(individual_18$highest_level_educ), width = 20)  # Wrap x-axis labels to 20 characters
  ) +
  scale_y_continuous(
    breaks = seq(0, max(table(individual_18$highest_level_educ)), by = 25)  # Add y-axis ticks every 25
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
# Save the education plot to data/03-figures_data
ggsave(
  filename = here("data/03-figures_data", "educ_individual_plot.png"),  # Saving as a PNG file
  plot = educ_individual_plot,
  width = 8, height = 6
)



## Informed ##
# Create a histogram to show the distribution of extent informed
# Create a new variable informed_18_plot to store the processed data for plotting
informed_18_plot <- individual_18 %>%
  mutate(extent_consider_informed = factor(extent_consider_informed, 
                                           levels = c("Extremely informed", 
                                                      "Very informed", 
                                                      "Not very informed", 
                                                      "Not at all informed")))
# Create the plot with adjusted y-axis range
informed_18_plot <- ggplot(informed_18_plot, aes(x = extent_consider_informed, fill = extent_consider_informed)) +
  geom_bar(color = "black", alpha = 0.7) + 
  labs(
    title = "Distribution of Self-Reported Climate Change Knowledge (2018)",
    x = "Extent Informed about Climate Change",
    y = "Frequency"
  ) + 
  theme_minimal() +
  scale_x_discrete(
    breaks = levels(informed_18_plot$extent_consider_informed), # Set breaks according to the factor levels
    labels = str_wrap(levels(informed_18_plot$extent_consider_informed), width = 20)  # Wrap x-axis labels to 20 characters
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
print(informed_18_plot)
# Save the informed plot to data/03-figures_data
ggsave(
  filename = here("data/03-figures_data", "informed_individual_plot.png"),  # Saving as a PNG file
  plot = informed_18_plot,
  width = 8, height = 6 
)




## Likelihood ##
# Create a figure for likelihood of taking actions to address climate change
# Manipulate individual_18 but assign to a new variable
likelihood_data <- individual_18 %>%
  pivot_longer(cols = starts_with("likelihood_"), 
               names_to = "action", 
               values_to = "likelihood") %>%
  mutate(action = recode(action, 
                         "likelihood_home_improvement" = "Home Improvement",
                         "likelihood_reduce_hydro" = "Reduce Hydro Usage",
                         "likelihood_minimize_car" = "Minimize Car Use",
                         "likelihood_vehicle_electric" = "Electric / Hybrid Vehicle",
                         "likelihood_protein_alternative" = "Meat Alternatives",
                         "likelihood_reduce_waste" = "Reduce Waste",
                         "likelihood_green_product" = "Purchase Green Products",
                         "likelihood_short_distance" = "Walk / Cycle Short Distances",
                         "likelihood_sort_waste" = "Sort Waste Correctly")) %>%
  filter(!is.na(likelihood)) %>%
  mutate(likelihood = factor(likelihood, 
                             levels = c("Already doing this or have done this", 
                                        "Very likely", 
                                        "Somewhat likely", 
                                        "Somewhat unlikely", 
                                        "Very unlikely")))
# Calculate percentages
likelihood_18_plot_percent <- likelihood_data %>%
  group_by(action, likelihood) %>%
  tally() %>%
  group_by(action) %>%
  mutate(percentage = n / sum(n) * 100)

# Create the likelihood plot
likelihood_18_plot <- ggplot(likelihood_18_plot_percent, aes(x = action, fill = likelihood, y = percentage)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Likelihood of Taking Climate Change Actions",
       x = "Actions", y = "Percentage", fill = "Likelihood") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(
    values = c("Already doing this or have done this" = "forestgreen", 
               "Very likely" = "green", 
               "Somewhat likely" = "yellow", 
               "Somewhat unlikely" = "orange", 
               "Very unlikely" = "red")
  ) +
  geom_text(aes(label = paste0(round(percentage), "%")),
            position = position_stack(vjust = 0.5), size = 3)

# Print the plot
print(likelihood_18_plot)
# Save the likelihood plot to data/03-figures_data
ggsave(
  filename = here("data/03-figures_data", "likelihood_individual_plot.png"),  # Saving as a PNG file
  plot = likelihood_18_plot,
  width = 8, height = 6
)






# Create a figure illustrating reasons why people are not taking specific action to mitigate climate change
# TO DO: pls come back to. the issue is that each "reason"category is appearing
# to be the same weight.We need to look at how given a specific action (column), how we
# count up  non-NA values (reasons). the count is displaying wrong. 
# TO DO: renaming OG data - bad

# Pivot to long format
data_long <- individual_18 %>%
  pivot_longer(
    cols = starts_with("unlikelihood"),  # Select all the columns starting with "unlikelihood"
    names_to = c("action", "reason"),  # Separate action and reason
    names_pattern = "unlikelihood_(.*)_(.*)_18",  # Extract action and reason parts from the column name
    values_to = "response"  # Values will be stored under 'response'
  ) %>%
  filter(!is.na(response)) %>%  # Filter out rows with NA responses
  mutate(
    action = case_when(  # Clean the action labels to ensure we have only 9 distinct actions
      str_detect(action, "home_improvement") ~ "Home Improvement",
      str_detect(action, "reduce_waste") ~ "Reduce Waste",
      str_detect(action, "short_distance") ~ "Walk/Cycle Short Distances",
      str_detect(action, "vehicle_electric") ~ "Electric/Hybrid Vehicle",
      str_detect(action, "minimize_car") ~ "Minimize Car Use",
      str_detect(action, "green_product") ~ "Purchase Green Products",
      str_detect(action, "protein_alternative") ~ "Protein Alternatives",
      str_detect(action, "reduce_hydro") ~ "Reduce Hydro Usage",
      str_detect(action, "sort_waste") ~ "Sort Waste",
      TRUE ~ action  # Default case if the action is already correctly labeled
    )
  )

# Summarize counts by action and reason (correctly count non-NA responses)
data_summary <- data_long %>%
  group_by(action, reason) %>%  # Group by both action and reason
  summarise(count = n(), .groups = "drop")  # Count the frequency of each reason per action

# Create a stacked bar chart
ggplot(data_summary, aes(x = action, y = count, fill = reason)) +
  geom_bar(stat = "identity") +  # Use 'identity' to use actual counts
  labs(
    title = "Reasons for Unlikelihood of Taking Climate Change Actions",
    x = "Climate Change Action",
    y = "Frequency of Responses",
    fill = "Reason"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  scale_fill_manual(values = c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999"))  # Customize colors







# Figure for method of communication
# Select columns that start with 'delivery_method'
delivery_columns <- individual_18 %>%
  select(starts_with("delivery_method"))

# Create an empty data frame to store the results
data_summary <- data.frame(
  delivery_method = character(),
  count_non_no = integer(),
  count_no = integer(),
  percentage = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each column to count non-"no" and "no" responses
for (col in colnames(delivery_columns)) {
  
  # Count non-"no" responses
  non_no_count <- sum(delivery_columns[[col]] != "no", na.rm = TRUE)
  
  # Count "no" responses
  no_count <- sum(delivery_columns[[col]] == "no", na.rm = TRUE)
  
  # Calculate the percentage of non-"no" responses
  total_responses <- non_no_count + no_count
  percentage <- (non_no_count / total_responses) * 100
  
  # Add the result to the summary data frame
  data_summary <- data_summary %>%
    add_row(
      delivery_method = col,
      count_non_no = non_no_count,
      count_no = no_count,
      percentage = percentage
    )
}

# Rename the delivery method columns for readability
data_summary <- data_summary %>%
  mutate(
    delivery_method = case_when(
      str_detect(delivery_method, "toronto.ca_website") ~ "Toronto.ca website",
      str_detect(delivery_method, "events") ~ "City of Toronto events",
      str_detect(delivery_method, "twitter") ~ "Twitter",
      str_detect(delivery_method, "facebook") ~ "Facebook",
      str_detect(delivery_method, "instagram") ~ "Instagram",
      str_detect(delivery_method, "enewsletter_email") ~ "City of Toronto e-newsletters / email",
      str_detect(delivery_method, "councillor_communication") ~ "Councillor communications",
      str_detect(delivery_method, "advertising_campaigns") ~ "Advertising campaigns",
      str_detect(delivery_method, "brochures_pamphlets") ~ "Brochures, pamphlets",
      str_detect(delivery_method, "other") ~ "Other",
      str_detect(delivery_method, "not_interested_receiving") ~ "Not interested in receiving information",
      TRUE ~ delivery_method  # Default case if something unexpected
    )
  )

# Save the plot as 'communication_18_plot'
communication_18_plot <- ggplot(data_summary, aes(x = reorder(delivery_method, percentage), y = percentage, fill = delivery_method)) +
  geom_bar(stat = "identity") +  # Use 'identity' to use actual counts
  labs(
    title = "Preferred Methods of Communication for Climate Change and Climate Action Information",
    x = "Delivery Method",
    y = "Percentage of Responses",
    fill = "Delivery Method"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for readability
        axis.ticks.y = element_line(),  # Add tick marks on the y-axis
        axis.ticks.x = element_line(),  # Add tick marks on the x-axis
        axis.text.y = element_text(size = 10),  # Adjust y-axis text size if needed
        legend.position = "none") +  # Remove the legend
  scale_fill_brewer(palette = "Set3") +  # Use a nice color palette for fill
  scale_y_continuous(breaks = seq(0, 100, by = 10))  # Set y-axis tick marks every 10%

# The plot is now saved as 'communication_18_plot'
communication_18_plot
# Save the communication method plot to data/03-figures_data
ggsave(
  filename = here("data/03-figures_data", "communication_18_plot.png"),  # Saving as a PNG file
  plot = communication_18_plot,
  width = 8, height = 6
)

                                


 
### 2018 summary tables + graphs ###

age_summary_18_table

education_summary_18_table

informed_summary_18_table

likelihood_summary_18_table

reason_summary_18_table
# TO DO: not functioning yet

communication_summary_18_table

#TO DO: decide which graphs to do 



### 2021 summary tables + graphs ###

age_summary_21_table

education_summary_21_table

informed_summary_21_table

likelihood_summary_21_table

reasons_summary_21_table

communication_summary_21_table

# TO DO: decide which graphs to do 




#### 2018 vs. 2021 summary graph ####

# TO DO: decide on comparison graphs

