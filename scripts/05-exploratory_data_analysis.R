#### Preamble ####
# Purpose: Exploring 2018 individual data as well as 2018 and 2021 summary data. 
# Author: Lexi Knight
# Date: 27 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete 01-download_data.R and 03-clean_data.R in scripts folder in order to access raw data.


#### Workspace setup ####

# Load necessary libraries
library(tidyverse) # core functions for data manipulation
library(here)      # constructs file paths
library(arrow)     # reads parquet files
library(forcats)   # reorders factor levels and factor-related manipulations
library(stringr)   # used in str-wrap to wrap long text labels on axes
library(dplyr)     # manipulating data; filtering, grouping, mutating and recording
library(ggplot2)   # creating visualizations; stacked bar plots and facet plots
library(tidyr)     # reshaping data with pivot_longer function
library(readr)     # managing csv files


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
    x = "Age (years)",
    y = "Frequency"
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
  width = 5, height = 2.5
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
    plot.margin = margin(10, 10, 20, 10),  # Increase the margins for better spacing
    legend.position = "none"  # Remove the legend
  ) +
  scale_fill_manual(values = c("High school or less" = "red", 
                               "Some community college, vocational, trade school" = "orange", 
                               "Completed community college, vocational, trade school" = "yellow", 
                               "Some university" = "lightgreen", 
                               "Completed undergraduate degree" = "green", 
                               "Post graduate/professional school" = "forestgreen", 
                               "Prefer not to answer" = "gray"))

# Display the plot
print(educ_individual_plot)

# Save the education plot to data/03-figures_data
ggsave(
  filename = here("data/03-figures_data", "educ_individual_plot.png"),  # Saving as a PNG file
  plot = educ_individual_plot,
  width = 5, height = 3.5
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
    title = "Distribution of Self-Reported Climate Change Knowledge",
    x = "Extent Informed about Climate Change",
    y = "Frequency",
    caption = "Figure 3: The histogram shows the distribution of self-reported climate change knowledge\namong 2018 survey respondents. This distribution provides insights into public awareness\nand the extent to which individuals feel informed about climate change."
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
    plot.margin = margin(10, 10, 40, 10),  # Further increase bottom margin for caption space
    plot.caption = element_text(hjust = 0.6, size = 8, lineheight = 1.2)  # Shift caption slightly to the right and reduce size
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
  width = 6, height = 4, dpi = 300  # Increased resolution for better clarity
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
  width = 6, height = 4
)


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
      str_detect(delivery_method, "events") ~ "City events",
      str_detect(delivery_method, "twitter") ~ "Twitter",
      str_detect(delivery_method, "facebook") ~ "Facebook",
      str_detect(delivery_method, "instagram") ~ "Instagram",
      str_detect(delivery_method, "enewsletter_email") ~ "City e-newsletters / email",
      str_detect(delivery_method, "councillor_communication") ~ "Councillor communications",
      str_detect(delivery_method, "advertising_campaigns") ~ "Advertising campaigns",
      str_detect(delivery_method, "brochures_pamphlets") ~ "Brochures, pamphlets",
      str_detect(delivery_method, "other") ~ "Other",
      str_detect(delivery_method, "not_interested_receiving") ~ "Not interested",
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
    fill = "Delivery Method",
    caption = "Figure 4: The stacked bar plot illustrates the preferred methods of communication for receiving\ninformation about climate change and climate action."
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels for readability
    axis.ticks.y = element_line(),  # Add tick marks on the y-axis
    axis.ticks.x = element_line(),  # Add tick marks on the x-axis
    axis.text.y = element_text(size = 10),  # Adjust y-axis text size if needed
    legend.position = "none",  # Remove the legend
    plot.caption = element_text(hjust = 0, size = 8, lineheight = 1.2),  # Center the caption
    plot.title = element_text(size = 10),  # Reduce the title font size
    plot.margin = margin(10, 10, 40, 10)  # Increase margin for caption space
  ) +
  scale_fill_brewer(palette = "Set3") +  # Use a nice color palette for fill
  scale_y_continuous(breaks = seq(0, 100, by = 10))  # Set y-axis tick marks every 10%

# The plot is now saved as 'communication_18_plot'
communication_18_plot

# Save the communication method plot to data/03-figures_data
ggsave(
  filename = here("data/03-figures_data", "communication_18_plot.png"),  # Saving as a PNG file
  plot = communication_18_plot,
  width = 6, height = 4
)






## likelihood by age ##
# Create age groups and remove NA categories
individual_18 <- individual_18 %>%
  mutate(age_group = cut(age, breaks = c(15, 24, 34, 44, 54, 64, 100), 
                         labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))) %>%
  filter(!is.na(age_group))

# Step 2: Pivot and recode data for likelihood of taking action
likelihood_age_data <- individual_18 %>%
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

# Calculate percentages for each action and age group
likelihood_age_plot_percent <- likelihood_age_data %>%
  group_by(action, age_group, likelihood) %>%
  tally() %>%
  group_by(action, age_group) %>%
  mutate(percentage = n / sum(n) * 100)

# Plot using faceting to show each action in a separate facet
likelihood_age_plot <- ggplot(likelihood_age_plot_percent, 
                              aes(x = age_group, fill = likelihood, y = percentage)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Likelihood of Taking Climate Change Actions by Age Group",
       x = "Age Group", 
       y = "Percentage", 
       fill = "Likelihood") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels
    axis.text.y = element_text(size = 10),  # Adjust font size for y-axis labels
    strip.text.x = element_text(size = 10),  # Adjust font size for facet labels
    legend.position = "right",  # Legend on the right side
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 9)  # Adjust legend text size
  ) +
  scale_fill_manual(
    values = c(
      "Already doing this or have done this" = "forestgreen", 
      "Very likely" = "green", 
      "Somewhat likely" = "yellow", 
      "Somewhat unlikely" = "orange", 
      "Very unlikely" = "red"
    )
  ) +
  facet_wrap(~action, scales = "free_y")  # Separate facets for each action

# Print the Plot
print(likelihood_age_plot)

# Save the Plot as a PNG
ggsave(
  filename = here("data/03-figures_data", "likelihood_age_plot.png"),
  plot = likelihood_age_plot,
  width = 9, height = 7
)




## likelihood by education ##
## Prepare the Data with Shortened and Ordered Education Levels
likelihood_education_data <- individual_18 %>%
  pivot_longer(
    cols = starts_with("likelihood_"), 
    names_to = "action", 
    values_to = "likelihood"
  ) %>%
  mutate(
    action = recode(action, 
                    "likelihood_home_improvement" = "Home Improvement",
                    "likelihood_reduce_hydro" = "Reduce Hydro Usage",
                    "likelihood_minimize_car" = "Minimize Car Use",
                    "likelihood_vehicle_electric" = "Electric / Hybrid Vehicle",
                    "likelihood_protein_alternative" = "Meat Alternatives",
                    "likelihood_reduce_waste" = "Reduce Waste",
                    "likelihood_green_product" = "Purchase Green Products",
                    "likelihood_short_distance" = "Walk / Cycle Short Distances",
                    "likelihood_sort_waste" = "Sort Waste Correctly")
  ) %>%
  filter(!is.na(likelihood)) %>%
  mutate(
    likelihood = factor(likelihood, 
                        levels = c("Already doing this or have done this", 
                                   "Very likely", 
                                   "Somewhat likely", 
                                   "Somewhat unlikely", 
                                   "Very unlikely")),
    highest_level_educ = factor(
      recode(
        highest_level_educ,
        "High school or less" = "High School",
        "Some community college, vocational, trade school" = "Some college / trade",
        "Completed community college, vocational, trade school" = "College / trade",
        "Some univeristy" = "Some university",
        "Completed undergraduate degree" = "Undergrad",
        "Post graduate/professional school" = "Post grad",
        "Prefer not to answer" = "Pref no answer"
      ),
      levels = c(
        "Pref no answer",
        "High School",
        "Some college / trade",
        "College / trade",
        "Some university",
        "Undergrad",
        "Post grad"
      )  # Explicitly set factor levels for ordering
    )
  )

## Group and Calculate Percentages by Education Level
likelihood_education_plot_percent <- likelihood_education_data %>%
  group_by(highest_level_educ, action, likelihood) %>%
  tally() %>%
  group_by(highest_level_educ, action) %>%
  mutate(percentage = n / sum(n) * 100)

## Create the Plot with Custom Order and Legend on the Right
likelihood_education_plot <- ggplot(likelihood_education_plot_percent, 
                                    aes(x = percentage, 
                                        y = highest_level_educ, 
                                        fill = likelihood)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
  labs(
    title = "Likelihood of Taking Climate Change Actions by Education Level",
    x = "Percentage",
    y = "Education Level",
    fill = "Likelihood"
  ) +
  facet_wrap(~action, scales = "free_x", ncol = 3) +  # Separate plots for each action, 3 columns
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels 45 degrees
    axis.text.y = element_text(size = 10),  # Adjust font size for y-axis labels
    strip.text.x = element_text(size = 12),  # Adjust font size for facet labels (action names)
    plot.margin = margin(10, 10, 20, 10),  # Increase plot margin for better spacing
    panel.spacing = unit(1, "lines"),  # Adjust spacing between facets
    legend.position = "right",  # Move legend to the right
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 9),  # Adjust legend text size
    strip.background = element_blank(),  # Remove background of facet labels
    strip.text = element_text(margin = margin(t = 10))  # Adjust strip margin for labels
  ) +
  scale_fill_manual(
    values = c("Already doing this or have done this" = "forestgreen", 
               "Very likely" = "green", 
               "Somewhat likely" = "yellow", 
               "Somewhat unlikely" = "orange", 
               "Very unlikely" = "red")
  ) +
  coord_flip() +  # Flip the axes
  theme(
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
  )

# Display the Plot
print(likelihood_education_plot)

# Save the Plot as a PNG
ggsave(
  filename = here("data/03-figures_data", "likelihood_education_plot.png"),
  plot = likelihood_education_plot,
  width = 9, height = 9
)




## Education 
# Load necessary libraries
library(kableExtra)
library(here)
library(dplyr)
library(readr)

# Read the 2018 and 2021 education summary tables from Parquet files
education_summary_18 <- read_parquet(here("data/03-figures_data", "education_summary_2018_table.parquet"))
education_summary_21 <- read_parquet(here("data/03-figures_data", "education_summary_2021_table.parquet"))

# Rename columns for clarity
colnames(education_summary_18) <- c("Education Level (2018)", "2018 (%)")
colnames(education_summary_21) <- c("Education Level (2021)", "2021 (%)")

# Specify the desired order for the 2018 education levels
desired_order_18 <- c(
  "High school or less", 
  "Some community college, vocational, trade school",
  "Completed community college, vocational, trade school",
  "Some university", 
  "Completed undergraduate degree",
  "Post graduate/professional school",
  "Prefer not to answer"
)

# Reorder the 2018 education levels based on the specified order
education_summary_18 <- education_summary_18 %>%
  mutate(`Education Level (2018)` = factor(`Education Level (2018)`, levels = desired_order_18)) %>%
  arrange(`Education Level (2018)`)

# Create a full join using row numbers to align both years even with different categories
education_summary_combined <- full_join(
  education_summary_18 %>% mutate(RowNum = row_number()),
  education_summary_21 %>% mutate(RowNum = row_number()),
  by = "RowNum"
) %>%
  select(-RowNum)  # Drop the helper column

# Convert factor columns to character to avoid NA assignment errors
education_summary_combined <- education_summary_combined %>%
  mutate(across(everything(), as.character))

# Replace NAs with empty strings to leave rows empty
education_summary_combined[is.na(education_summary_combined)] <- ""

# Render the table with kableExtra for proper formatting and scaling
education_summary_combined %>%
  kbl() %>%  # Create the table with kableExtra
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1, width = "15em") %>%  # Adjust width of the first column (adjust if necessary)
  row_spec(0, font_size = 10)  # Optional: Adjust font size for the header row

# Save the combined table as a CSV file
write.csv(education_summary_combined, here("data/03-figures_data", "education_summary_combined.csv"), row.names = FALSE)





## Informed 
# Load the 2018 and 2021 Extent Informed summary tables from Parquet files
informed_summary_18 <- read_parquet(here("data/03-figures_data", "informed_summary_2018_table.parquet"))
informed_summary_21 <- read_parquet(here("data/03-figures_data", "informed_summary_2021_table.parquet"))

# Rename columns for clarity
colnames(informed_summary_18) <- c("Extent Informed (2018)", "2018 (%)")
colnames(informed_summary_21) <- c("Extent Informed (2021)", "2021 (%)")

# Specify the desired order for the 2018 Extent Informed categories
desired_order_18 <- c("Extremely informed", "Very informed", "Not very informed", "Not at all informed")

# Reorder the 2018 Extent Informed categories based on the specified order
informed_summary_18 <- informed_summary_18 %>%
  mutate(`Extent Informed (2018)` = factor(`Extent Informed (2018)`, levels = desired_order_18)) %>%
  arrange(`Extent Informed (2018)`)

# Create a full join using row numbers to align both years (no missing values)
informed_summary_combined <- full_join(
  informed_summary_18 %>% mutate(RowNum = row_number()),
  informed_summary_21 %>% mutate(RowNum = row_number()),
  by = "RowNum"
) %>%
  select(-RowNum)  # Drop the helper column

# Convert factor columns to character to avoid NA assignment errors
informed_summary_combined <- informed_summary_combined %>%
  mutate(across(everything(), as.character))

# Replace NAs with empty strings to leave rows empty
informed_summary_combined[is.na(informed_summary_combined)] <- ""

# Render the combined table using tinytable
informed_summary_combined_table <- tt(informed_summary_combined)

# Print the table
print(informed_summary_combined_table)

# Save the combined table as a Parquet file
write_parquet(informed_summary_combined, here("data/03-figures_data", "informed_summary_combined_table.parquet"))




## Communication
# Load the 2018 and 2021 Communication summary tables from Parquet files
communication_summary_18 <- read_parquet(here("data/03-figures_data", "communication_summary_2018_table.parquet"))
communication_summary_21 <- read_parquet(here("data/03-figures_data", "communication_summary_2021_table.parquet"))

# Rename columns for clarity
colnames(communication_summary_18) <- c("Communication Method_2018", "Percentage_2018")
colnames(communication_summary_21) <- c("Communication Method_2021", "Percentage_2021")

# Reorder the 2018 communication methods (if needed)
# Specify the desired order for the 2018 communication methods
desired_order_18 <- c(
  "Toronto.ca website",
  "Events",
  "Twitter",
  "Facebook",
  "Instagram",
  "Enewsletter / email",
  "Councillor communication",
  "Advertising campaigns",
  "Brochures / Pamphlets",
  "Other",
  "Not interested"
)

# Reorder the 2018 communication methods based on the specified order
communication_summary_18 <- communication_summary_18 %>%
  mutate(`Communication Method_2018` = factor(`Communication Method_2018`, levels = desired_order_18)) %>%
  arrange(`Communication Method_2018`)

# Rename categories for 2021 Communication Methods
# Define the mapping for 2021 communication methods
communication_method_rename_map_21 <- list(
  "City of Toronto events" = "Events",
  "City of Toronto e-newsletters / email" = "Enewsletter / email",
  "Councillor e-newsletters" = "Councillor communication",
  "Printed or online brochures, pamphlets" = "Brochures / Pamphlets",
  "Other" = "Other Methods",
  "Not interested in receiving information" = "Not interested",
  "Other Methods" = "Other"
)
# Apply renaming based on the mapping for 2021
communication_summary_21 <- communication_summary_21 %>%
  mutate(`Communication Method_2021` = recode(`Communication Method_2021`, !!!communication_method_rename_map_21))

# Specify the desired order for the 2021 communication methods
desired_order_21 <- c(
  "Toronto.ca website",
  "Events",
  "Twitter",
  "Facebook",
  "Instagram",
  "Enewsletter / email",
  "Councillor communication",
  "Advertising campaigns",
  "Brochures / Pamphlets",
  "Other Methods",
  "Not interested",
  "BetterHomesTO.ca website",
  "Mail/ letter",
  "Nothing",
  "Don't know"
)

# Reorder the 2021 communication methods based on the specified order
communication_summary_21 <- communication_summary_21 %>%
  mutate(`Communication Method_2021` = factor(`Communication Method_2021`, levels = desired_order_21)) %>%
  arrange(`Communication Method_2021`)

# Create a full join using row numbers to align both years even with different categories
communication_summary_combined <- full_join(
  communication_summary_18 %>% mutate(RowNum = row_number()),
  communication_summary_21 %>% mutate(RowNum = row_number()),
  by = "RowNum"
) %>%
  select(-RowNum)  # Drop the helper column

# Convert factor columns to character to avoid NA assignment errors
communication_summary_combined <- communication_summary_combined %>%
  mutate(across(everything(), as.character))

# Replace NAs with empty strings to leave rows empty
communication_summary_combined[is.na(communication_summary_combined)] <- ""

# Render the combined table using tinytable (4 columns: Communication Method_2018, Percentage_2018, Communication Method_2021, Percentage_2021)
communication_summary_combined_table <- tt(communication_summary_combined, 
                                           row.names = FALSE, 
                                           col.names = c("2018 Communication Method", "2018 Percentage", "2021 Communication Method", "2021 Percentage"), 
                                           escape = FALSE)

# Print the table
print(communication_summary_combined_table)

# Save the combined table as a new Parquet file
write_parquet(communication_summary_combined, here("data/03-figures_data", "communication_summary_combined_renamed_table.parquet"))

