#### Preamble ####
# Purpose: Cleans the raw climate perceptions data recorded to prepare it for analysis.
# Author: Lexi Knight
# Date: 10 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete 01-download_data.R in scripts folder in order to access raw data.


#### Workspace setup ####
# install necessary packages
# install.packages(c("readr", "dplyr", "openxlsx", "readxl", "arrow"))

# load necessary packages
library(readr)
library(dplyr) # for combining data frames
library(openxlsx)
library(readxl)
library(arrow) # for saving file as parquet

#### Clean data 2018 ####
# Read the csv file
twenty_eighteen_raw_data <- read_csv("data/01-raw_data/twenty_eighteen_raw_data.csv")

# Sample size information
sample_size_2018 <- 404 # This is the sample size for the year 2018

#  I want to select certain columns outlined below
twenty_eighteen <- twenty_eighteen_raw_data %>%
  select(HIDAGE1, Q2, Q10r1, Q10r2, Q10r3, Q10r4, Q10r5, Q10r6, Q10r7, Q10r8, Q10r9,
  Q11_Lr1r1, Q11_Lr1r2, Q11_Lr1r3, Q11_Lr1r4, Q11_Lr1r5, Q11_Lr1r6, Q11_Lr1r7, Q11_Lr1r8,
  Q11_Lr2r1, Q11_Lr2r2, Q11_Lr2r3, Q11_Lr2r4, Q11_Lr2r5, Q11_Lr2r6, Q11_Lr2r7, Q11_Lr2r8,
  Q11_Lr3r1, Q11_Lr3r2, Q11_Lr3r3, Q11_Lr3r4, Q11_Lr3r5, Q11_Lr3r6, Q11_Lr3r7, Q11_Lr3r8,
  Q11_Lr4r1, Q11_Lr4r2, Q11_Lr4r3, Q11_Lr4r4, Q11_Lr4r5, Q11_Lr4r6, Q11_Lr4r7, Q11_Lr4r8,
  Q11_Lr5r1, Q11_Lr5r2, Q11_Lr5r3, Q11_Lr5r4, Q11_Lr5r5, Q11_Lr5r6, Q11_Lr5r7, Q11_Lr5r8,
  Q11_Lr6r1, Q11_Lr6r2, Q11_Lr6r3, Q11_Lr6r4, Q11_Lr6r5, Q11_Lr6r6, Q11_Lr6r7, Q11_Lr6r8,
  Q11_Lr7r1, Q11_Lr7r2, Q11_Lr7r3, Q11_Lr7r4, Q11_Lr7r5, Q11_Lr7r6, Q11_Lr7r7, Q11_Lr7r8,
  Q11_Lr8r1, Q11_Lr8r2, Q11_Lr8r3, Q11_Lr8r4, Q11_Lr8r5, Q11_Lr8r6, Q11_Lr8r7, Q11_Lr8r8,
  Q11_Lr9r1, Q11_Lr9r2, Q11_Lr9r3, Q11_Lr9r4, Q11_Lr9r5, Q11_Lr9r6, Q11_Lr9r7, Q11_Lr9r8,
  Q13r1, Q13r2, Q13r3, Q13r4, Q13r5, Q13r6, Q13r7, Q13r8, Q13r9, Q13r10, Q13r11,
  QD5)

## Rename columns 2018 ##
# use rename() function to give more meaningful names
renamed_twenty_eighteen <- twenty_eighteen %>%
  rename(
    age_18 = HIDAGE1,
    extent_consider_informed_18 = Q2,
    likelihood_action_home_improvement_18= Q10r1,
    likelihood_action_reduce_hydro_18 = Q10r2,
    likelihood_action_minimize_car_18 = Q10r3,
    likelihood_action_vehicle_electric_18 = Q10r4,
    likelihood_action_protein_alternative_18 = Q10r5,
    likelihood_action_reduce_waste_18 = Q10r6,
    likelihood_action_green_product_18 = Q10r7,
    likelihood_action_short_distance_18 = Q10r8,
    likelihood_action_sort_waste_18 = Q10r9,
    unlikelihood_action_home_improvement_confusing_18 = Q11_Lr1r1,
    unlikelihood_action_home_improvement_individual_difference_18 = Q11_Lr1r2,
    unlikelihood_action_home_improvement_ineffective_18 = Q11_Lr1r3,
    unlikelihood_action_home_improvement_costly_18 = Q11_Lr1r4,
    unlikelihood_action_home_improvement_unavailable_18 = Q11_Lr1r5,
    unlikelihood_action_home_improvement_inconvenient_18 = Q11_Lr1r6,
    unlikelihood_action_home_improvement_uninterested_18 = Q11_Lr1r7,
    unlikelihood_action_home_improvement_other_18 = Q11_Lr1r8,
    unlikelihood_action_reduce_hydro_confusing_18 = Q11_Lr2r1,
    unlikelihood_action_reduce_hydro_individual_difference_18 = Q11_Lr2r2,
    unlikelihood_action_reduce_hydro_ineffective_18 = Q11_Lr2r3,
    unlikelihood_action_reduce_hydro_costly_18 = Q11_Lr2r4,
    unlikelihood_action_reduce_hydro_unavailable_18 = Q11_Lr2r5,
    unlikelihood_action_reduce_hydro_inconvenient_18 = Q11_Lr2r6,
    unlikelihood_action_reduce_hydro_uninterested_18 = Q11_Lr2r7,
    unlikelihood_action_reduce_hydro_other_18 = Q11_Lr2r8,
    unlikelihood_action_minimize_car_confusing_18 = Q11_Lr3r1,
    unlikelihood_action_minimize_car_individual_difference_18 = Q11_Lr3r2,
    unlikelihood_action_minimize_car_ineffective_18 = Q11_Lr3r3,
    unlikelihood_action_minimize_car_costly_18 = Q11_Lr3r4,
    unlikelihood_action_minimize_car_unavailable_18 = Q11_Lr3r5,
    unlikelihood_action_minimize_car_inconvenient_18 = Q11_Lr3r6,
    unlikelihood_action_minimize_car_uninterested_18 = Q11_Lr3r7,
    unlikelihood_action_minimize_car_other_18 = Q11_Lr3r8,
    unlikelihood_action_vehicle_electric_confusing_18 = Q11_Lr4r1,
    unlikelihood_action_vehicle_electric_individual_difference_18 = Q11_Lr4r2,
    unlikelihood_action_vehicle_electric_ineffective_18 = Q11_Lr4r3,
    unlikelihood_action_vehicle_electric_costly_18 = Q11_Lr4r4,
    unlikelihood_action_vehicle_electric_unavailable_18 = Q11_Lr4r5,
    unlikelihood_action_vehicle_electric_inconvenient_18 = Q11_Lr4r6,
    unlikelihood_action_vehicle_electric_uninterested_18 = Q11_Lr4r7,
    unlikelihood_action_vehicle_electric_other_18 = Q11_Lr4r8,
    unlikelihood_action_protein_alternative_confusing_18 = Q11_Lr5r1,
    unlikelihood_action_protein_alternative_individual_difference_18 = Q11_Lr5r2,
    unlikelihood_action_protein_alternative_ineffective_18 = Q11_Lr5r3,
    unlikelihood_action_protein_alternative_costly_18 = Q11_Lr5r4,
    unlikelihood_action_protein_alternative_unavailable_18 = Q11_Lr5r5,
    unlikelihood_action_protein_alternative_inconvenient_18 = Q11_Lr5r6,
    unlikelihood_action_protein_alternative_uninterested_18 = Q11_Lr5r7,
    unlikelihood_action_protein_alternative_other_18 = Q11_Lr5r8,
    unlikelihood_action_reduce_waste_confusing_18 = Q11_Lr6r1,
    unlikelihood_action_reduce_waste_individual_difference_18 = Q11_Lr6r2,
    unlikelihood_action_reduce_waste_ineffective_18 = Q11_Lr6r3,
    unlikelihood_action_reduce_waste_costly_18 = Q11_Lr6r4,
    unlikelihood_action_reduce_waste_unavailable_18 = Q11_Lr6r5,
    unlikelihood_action_reduce_waste_inconvenient_18 = Q11_Lr6r6,
    unlikelihood_action_reduce_waste_uninterested_18 = Q11_Lr6r7,
    unlikelihood_action_reduce_waste_other_18 = Q11_Lr6r8,
    unlikelihood_action_green_product_confusing_18 = Q11_Lr7r1,
    unlikelihood_action_green_product_individual_difference_18 = Q11_Lr7r2,
    unlikelihood_action_green_product_ineffective_18 = Q11_Lr7r3,
    unlikelihood_action_green_product_costly_18 = Q11_Lr7r4,
    unlikelihood_action_green_product_unavailable_18 = Q11_Lr7r5,
    unlikelihood_action_green_product_inconvenient_18 = Q11_Lr7r6,
    unlikelihood_action_green_product_uninterested_18 = Q11_Lr7r7,
    unlikelihood_action_green_product_other_18 = Q11_Lr7r8,
    unlikelihood_action_short_distance_confusing_18 = Q11_Lr8r1,
    unlikelihood_action_short_distance_individual_difference_18 = Q11_Lr8r2,
    unlikelihood_action_short_distance_ineffective_18 = Q11_Lr8r3,
    unlikelihood_action_short_distance_costly_18 = Q11_Lr8r4,
    unlikelihood_action_short_distance_unavailable_18 = Q11_Lr8r5,
    unlikelihood_action_short_distance_inconvenient_18 = Q11_Lr8r6,
    unlikelihood_action_short_distance_uninterested_18 = Q11_Lr8r7,
    unlikelihood_action_short_distance_other_18 = Q11_Lr8r8,
    unlikelihood_action_sort_waste_confusing_18 = Q11_Lr9r1,
    unlikelihood_action_sort_waste_individual_difference_18 = Q11_Lr9r2,
    unlikelihood_action_sort_waste_ineffective_18 = Q11_Lr9r3,
    unlikelihood_action_sort_waste_costly_18 = Q11_Lr9r4,
    unlikelihood_action_sort_waste_unavailable_18 = Q11_Lr9r5,
    unlikelihood_action_sort_waste_inconvenient_18 = Q11_Lr9r6,
    unlikelihood_action_sort_waste_uninterested_18 = Q11_Lr9r7,
    unlikelihood_action_sort_waste_other_18 = Q11_Lr9r8,
    delivery_method_toronto.ca_website_18 = Q13r1,
    delivery_method_events_18 = Q13r2,
    delivery_method_twitter_18 = Q13r3,
    delivery_method_facebook_18 = Q13r4,
    delivery_method_instagram_18 = Q13r5,
    delivery_method_enewsletter_email_18 = Q13r6,
    delivery_method_councillor_communication_18 = Q13r7,
    delivery_method_advertising_campaigns_18 = Q13r8,
    delivery_method_brochures_pamphlets_18 = Q13r9,
    delivery_method_other_18 = Q13r10,
    delivery_method_not_interested_receiving_18 = Q13r11,
    highest_level_educ_18 = QD5
  )

# Fix all "unlikelihood" and "delivery" columns
# Replace NA, "NO TO", and other values in columns
fixed_twenty_eighteen <- renamed_twenty_eighteen %>%
  mutate(
    across(
      starts_with("unlikelihood_action"),
      ~ ifelse(is.na(.), "no",
               ifelse(startsWith(., "NO TO"), "no", "yes"))
    )
  ) %>%
  mutate(
    across(
      starts_with("delivery_method"),
      ~ ifelse(is.na(.), "no",
               ifelse(startsWith(., "NO TO"), "no", "yes"))
    )
  )


# View the first few rows to confirm the data
head(fixed_twenty_eighteen)

#### Save 2018 individual data ####
write_parquet(fixed_twenty_eighteen, "data/02-analysis_data/twenty_eighteen_individual_analysis_data.parquet")




#### Clean data 2021 ####

# File path to the saved workbook
file_path <- "data/01-raw_data/twenty_twenty_one_raw_data.xlsx"

# Sample size information
sample_size_2021 <- 1401 # This is the sample size for the year 2021

## Age - sheet 3 ##

# Extract data from Sheet 3 + 1 (accounting for the index sheet)
sheet_data <- read_excel(file_path, sheet = 4)

# Define rows and columns for Sheet 3 that want extract 
rows_A <- c(7, 10, 13, 16)  # Categories in column A (age categories)
rows_B <- c(8, 11, 14, 17)  # Data in column B

# Extract the age categories from column A
categories <- sheet_data %>% 
  slice(rows_A) %>% 
  pull(1)  # Pull the first column as a vector

# Remove unexpected white spaces or non-printing characters
categories <- trimws(categories)

# Print categories to check
print("Categories (Column A):")
print(categories)

# Extract the percentage raw data from column B 
raw_data_B <- sheet_data[rows_B, 2]

# Convert raw data from tibble to character and then to numeric
raw_data_B_numeric <- as.numeric(as.character(raw_data_B[[1]]))

# Multiply by 100 to convert to percentages (if the conversion succeeded)
percentages <- raw_data_B_numeric * 100

# Print the consolidated percentages
# print("Percentages (Converted and multiplied by 100):")
# print(percentages)

## Save the age_summary_21 ##

# Combine categories and percentages into a data frame
age_summary_21 <- data.frame(
  Category = categories,
  Percentage = percentages
)

# Rename the column headers
colnames(age_summary_21) <- c("Age Category", "Percentage")

# Print the consolidated data to check
print(age_summary_21)





## education - sheet 11 ##

# Extract data from Sheet 11 + 1 (accounting for the index sheet)
sheet_data <- read_excel(file_path, sheet = 12)

# Define rows and columns for Sheet 11 that want extract 
rows_A <- c(7, 10, 13, 16, 19, 22)  # Categories in column A (education categories)
rows_B <- c(8, 11, 14, 17, 20, 23)  # Data in column B

# Extract the education categories from column A
categories <- sheet_data %>% 
  slice(rows_A) %>% 
  pull(1)  # Pull the first column as a vector

# Remove unexpected white spaces or non-printing characters
categories <- trimws(categories)

# Print categories to check
print("Categories (Column A):")
print(categories)

# Extract the percentage raw data from column B 
raw_data_B <- sheet_data[rows_B, 2]

# Convert raw data from tibble to character and then to numeric
raw_data_B_numeric <- as.numeric(as.character(raw_data_B[[1]]))

# Multiply by 100 to convert to percentages (if the conversion succeeded)
percentages <- raw_data_B_numeric * 100

# Print the consolidated percentages
print("Percentages (Converted and multiplied by 100):")
print(percentages)

## Save the education_summary_21 ##

# Combine categories and percentages into a data frame
education_summary_21 <- data.frame(
  Category = categories,
  Percentage = percentages
)

# Rename the column headers
colnames(education_summary_21) <- c("Education Level", "Percentage")

# Print the consolidated data to check
print(education_summary_21)





## Extent Informed - sheet 34 ##

# Extract data from Sheet 34 + 1 (accounting for the index sheet)
sheet_data <- read_excel(file_path, sheet = 35)

# Define rows and columns for Sheet 11 that want extract 
rows_A <- c(7, 10, 13, 16)  # Categories in column A (extent informed categories)
rows_B <- c(8, 11, 14, 17)  # Data in column B

# Extract the extent informed categories from column A
categories <- sheet_data %>% 
  slice(rows_A) %>% 
  pull(1)  # Pull the first column as a vector

# Remove unexpected white spaces or non-printing characters
categories <- trimws(categories)

# Print categories to check
print("Categories (Column A):")
print(categories)

# Extract the percentage raw data from column B 
raw_data_B <- sheet_data[rows_B, 2]

# Convert raw data from tibble to character and then to numeric
raw_data_B_numeric <- as.numeric(as.character(raw_data_B[[1]]))

# Multiply by 100 to convert to percentages (if the conversion succeeded)
percentages <- raw_data_B_numeric * 100

# Print the consolidated percentages
print("Percentages (Converted and multiplied by 100):")
print(percentages)

## Save the informed_summary_21 ##

# Combine categories and percentages into a data frame
informed_summary_21 <- data.frame(
  Category = categories,
  Percentage = percentages
)

# Rename the column headers
colnames(informed_summary_21) <- c("Extent Informed", "Percentage")

# Print the consolidated data to check
print(informed_summary_21)





## Likelihood Take Action - sheet 69 ##

# Extract data from Sheet 69 + 1 (accounting for the index sheet)
sheet_data <- read_excel(file_path, sheet = 70)

# Define rows and columns for Sheet 11 that want extract 
rows_A <- c(5, 7, 9, 11, 13, 15, 17)  # Categories in column A (likelihood categories)
rows_B_to_P <- c(6, 8, 10, 12, 14, 16, 18)  # Rows for data in columns B through P

# Extract the likelihood take action categories from column A
categories <- sheet_data %>%
  slice(rows_A) %>%
  pull(1) %>%  # Pull the first column (A) as a vector
  trimws()  # Remove unexpected whitespace

# Rename the 13th category (index 5 in rows_A) to "Already doing/done in past year"
categories[5] <- "Already doing/done in past year"
# Rename the 17th category (index 7 in rows_A) to "Unsure"
categories[7] <- "Unsure"

# Print categories to check
print("Categories (Column A):")
print(categories)

# Initialize a list to store data for columns B through P
percentage_list <- list()

# Loop through columns B to P and extract corresponding data
for (col_index in 2:16) {  # Columns B (2) to P (16) in R indexing
  raw_data <- sheet_data[rows_B_to_P, col_index]
  
  # Convert raw data to character, replace "-" with "0", then convert to numeric
  raw_data_cleaned <- as.character(raw_data[[1]])
  raw_data_cleaned[raw_data_cleaned == "-"] <- "0"
  
  # Convert the cleaned data to numeric and multiply by 100 (if conversion succeeded)
  numeric_data <- as.numeric(raw_data_cleaned) * 100
  percentage_list[[paste0("Col_", LETTERS[col_index])]] <- numeric_data
}

# Combine categories and percentages into a data frame
likelihood_summary_21 <- data.frame(
  Category = categories,
  percentage_list
)

colnames(likelihood_summary_21) <- c(
  "Likelihood to Take Action",
  "Purchase energy efficient appliances",
  "Install a programmable thermostat",
  "Install LED lightbulbs",
  "Undertake major home renos for energy efficiency",
  "Add solar panels to home",
  "Get an EnerGuide home energy evaluation to identify opportunities",
  "Reduce water use",
  "Use public transit more",
  "Cycle more",
  "Walk more",
  "Purchase electric/hybrid vehicle in next 1-3 years",
  "Eat less meat",
  "Reduce amount of own waste",
  "Purchase environmentally friendly items",
  "Put effort into sorting waste into correct bins"
)

# Print the consolidated data to check
print("Likelihood Summary:")
print(likelihood_summary_21)





## Reason Unlikely to Take Action - sheet 85 ##

# Extract data from Sheet 85 + 1 (accounting for the index sheet)
sheet_data <- read_excel(file_path, sheet = 86)

# Define rows and columns for Sheet 11 that want extract 
rows_A <- c(5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33)  # Categories in column A (reason categories)
rows_B_to_P <- c(6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34)  # Rows for data in columns B through P

# Extract the likelihood take action categories from column A
categories <- sheet_data %>%
  slice(rows_A) %>%
  pull(1) %>%  # Pull the first column (A) as a vector
  trimws()  # Remove unexpected whitespace

# Print categories to check
print("Categories (Column A):")
print(categories)

# Initialize a list to store data for columns B through L
percentage_list <- list()

# Loop through columns B to L and extract corresponding data
for (col_index in 2:12) {  # Columns B (2) to L (12) in R indexing
  raw_data <- sheet_data[rows_B_to_P, col_index]
  
  # Convert raw data to character, replace "-" with "0", then convert to numeric
  raw_data_cleaned <- as.character(raw_data[[1]])
  raw_data_cleaned[raw_data_cleaned == "-"] <- "0"
  
  # Convert the cleaned data to numeric and multiply by 100 (if conversion succeeded)
  numeric_data <- as.numeric(raw_data_cleaned) * 100
  percentage_list[[paste0("Col_", LETTERS[col_index])]] <- numeric_data
}

# Combine categories and percentages into a data frame
reasons_summary_21 <- data.frame(
  Category = categories,
  percentage_list
)

colnames(reasons_summary_21) <- c(
  "Likelihood to Take Action",
  "Purchase energy efficient appliances",
  "Install a programmable thermostat",
  "Install LED lightbulbs",
  "Undertake major home renos for energy efficiency",
  "Add solar panels to home",
  "Get an EnerGuide home energy evaluation to identify opportunities",
  "Reduce water use",
  "Eat less meat",
  "Reduce amount of own waste",
  "Purchase environmentally friendly items",
  "Put effort into sorting waste into correct bins"
)

# Print the consolidated data to check
print("Reasons Summary:")
print(reasons_summary_21)





## City support - sheet 113 ##

# Extract data from Sheet 113 + 1 (accounting for the index sheet)
sheet_data <- read_excel(file_path, sheet = 114)

# Define rows and columns for Sheet 113 that want extract 
rows_A <- c(7, 10, 13, 16, 19,22, 25, 28, 31,34, 37,40, 43, 46, 49, 52, 55, 58,
            61, 64, 67, 70, 73, 76, 79, 82, 85, 88, 91, 94, 97, 100, 103, 106,
            109, 112,115, 118) # Categories in column A (city support categories)
rows_B <- c(8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50, 53, 56, 59,
           62, 65, 68, 71, 74, 77, 80, 83, 86, 89, 92, 95, 98, 101, 104, 107,
           110, 113, 116, 119) # Data in column B

# Extract the city support categories from column A
categories <- sheet_data %>% 
  slice(rows_A) %>% 
  pull(1)  # Pull the first column as a vector

# Remove unexpected white spaces or non-printing characters
categories <- trimws(categories)

# Print categories to check
print("Categories (Column A):")
print(categories)

# Extract the percentage raw data from column B 
raw_data_B <- sheet_data[rows_B, 2]

# Convert raw data from tibble to character and then to numeric
raw_data_B_numeric <- as.numeric(as.character(raw_data_B[[1]]))

# Multiply by 100 to convert to percentages (if the conversion succeeded)
percentages <- raw_data_B_numeric * 100

# Print the consolidated percentages
print("Percentages (Converted and multiplied by 100):")
print(percentages)

## Save the informed_summary_21 ##

# Combine categories and percentages into a data frame
support_summary_21 <- data.frame(
  Category = categories,
  Percentage = percentages
)

# Rename the column headers
colnames(support_summary_21) <- c("City Support to Motivate", "Percentage")

# Print the consolidated data to check
print(support_summary_21)







## Methods of communication - sheet 114 ##

# Extract data from Sheet 113 + 1 (accounting for the index sheet)
sheet_data <- read_excel(file_path, sheet = 115)

# Define rows and columns for Sheet 113 that want extract 
rows_A <- c(7, 10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49)
# Categories in column A (methods of communication)
rows_B <- c(8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50) 
# Data in column B

# Extract the city support categories from column A
categories <- sheet_data %>% 
  slice(rows_A) %>% 
  pull(1)  # Pull the first column as a vector

# Remove unexpected white spaces or non-printing characters
categories <- trimws(categories)

# Rename the 8th category (index 8 in rows_A)
categories[8] <- "Advertising campaigns"

# Print categories to check
print("Categories (Column A):")
print(categories)

# Extract the percentage raw data from column B 
raw_data_B <- sheet_data[rows_B, 2]

# Convert raw data from tibble to character and then to numeric
raw_data_B_numeric <- as.numeric(as.character(raw_data_B[[1]]))

# Multiply by 100 to convert to percentages (if the conversion succeeded)
percentages <- raw_data_B_numeric * 100

# Print the consolidated percentages
print("Percentages (Converted and multiplied by 100):")
print(percentages)

## Save the informed_summary_21 ##

# Combine categories and percentages into a data frame
communication_summary_21 <- data.frame(
  Category = categories,
  Percentage = percentages
)

# Rename the column headers
colnames(communication_summary_21) <- c("Method of Communication", "Percentage")

# Print the consolidated data to check
print(communication_summary_21)



## Combine Individual 2021 Data ##

# Add a source column to each dataset to identify their origin
age_summary_21 <- age_summary_21 %>% mutate(Source = "Age Summary 2021")
education_summary_21 <- education_summary_21 %>% mutate(Source = "Education Summary 2021")
informed_summary_21 <- informed_summary_21 %>% mutate(Source = "Informed Summary 2021")
likelihood_summary_21 <- likelihood_summary_21 %>% mutate(Source = "Likelihood Summary 2021")
reasons_summary_21 <- reasons_summary_21 %>% mutate(Source = "Reasons Summary 2021")
support_summary_21 <- support_summary_21 %>% mutate(Source = "Support Summary 2021")
communication_summary_21 <- communication_summary_21 %>% mutate(Source = "Communication Summary 2021")

# Combine all datasets into one
combined_summary_21 <- bind_rows(
  age_summary_21,
  education_summary_21,
  informed_summary_21,
  likelihood_summary_21,
  reasons_summary_21,
  support_summary_21,
  communication_summary_21
)

# Save the combined data as a Parquet file
write_parquet(combined_summary_21, "data/02-analysis_data/twenty_twenty_two_summary_analysis_data.parquet")


# Confirmation message
print("Data saved successfully!")






