#### Preamble ####
# Purpose: Cleans the raw climate perceptions data recorded to prepare it for analysis.
# Author: Lexi Knight
# Date: 10 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete 01-download_data.R in scripts folder in order to access raw data.


#### Workspace setup ####
# install necessary packages
# install.packages(c("readr", "dplyr", "openxlsx", "readxl"))

# load necessary packages
library(readr)
library(dplyr) # for combining data frames
library(openxlsx)
library(readxl)

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

#### Rename columns 2018 ####
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
write_csv(fixed_twenty_eighteen, "data/02-analysis_data/twenty_eighteen_individual_analysis_data.csv")





#### Clean data 2021 ####

# File path to the saved workbook
file_path <- "data/01-raw_data/twenty_twenty_one_raw_data.xlsx"

# Sample size information
sample_size_2021 <- 1401 # This is the sample size for the year 2021

# Extract data from Sheet 3 + 1 (accounting for the index sheet)
sheet_data <- read_excel(file_path, sheet = 4)

# Define rows and columns for Sheet 3 that want extract 
rows_A <- c(8, 11, 14, 17)  # Categories in column A (age categories)
rows_B <- c(8, 11, 14, 17)  # Data in column B (whole numbers)

# Extract the age categories from column A
categories <- sheet_data[rows_A, 1]

# Print categories to check if they are properly extracted
print("Categories (Column A):")
print(categories)

# Remove any unexpected white spaces or non-printing characters
categories <- trimws(categories)

# Print categories after trimming whitespace
print("Trimmed Categories (Column A):")
print(categories)

# Extract the percentage raw data from column B 
raw_data_B <- sheet_data[rows_B, 2]

# Convert raw data from tibble to character and then to numeric
raw_data_B_numeric <- as.numeric(as.character(raw_data_B[[1]]))

# Print the numeric conversion result to check
print("Converted Raw Data (as numeric):")
print(raw_data_B_numeric)

# Multiply by 100 to convert to percentages (if the conversion succeeded)
percentages <- raw_data_B_numeric * 100

# Print the percentages
print("Percentages (Converted and multiplied by 100):")
print(percentages)
# maybe fix categories - they are showing as NA






# i think not needed 

# Clean the raw data:
# Convert to character and remove unwanted characters or spaces
cleaned_data_B <- as.numeric(gsub("[^0-9.]", "", as.character(raw_data_B)))

# Check for any NAs in the cleaned data
if (any(is.na(cleaned_data_B))) {
  cat("There are missing or improperly formatted values in the data.")
} else {
  # Multiply by 100 to convert to percentages
  percentages <- cleaned_data_B * 100
  
  # Combine categories and percentages into a data frame
  final_data <- data.frame(
    Age = categories,
    Percentage = percentages
  )
  
  # Print the final table
  print(final_data)
}




# Sheet numbers to extract
sheet_numbers <- c(3, 11, 34, 69, 85, 113, 114)

#### Sheet-specific Row Parameters ####
sheet_params <- list(
  list(sheet = 3, rows_A = c(8, 11, 14, 17), rows_B = c(9, 12, 15, 18), cols_B_to = NULL),
  list(sheet = 11, rows_A = c(8, 11, 14, 17, 20, 23), rows_B = c(12, 15, 18, 21, 24), cols_B_to = NULL),
  list(sheet = 34, rows_A = c(8, 11, 14, 17), rows_B = c(9, 12, 15, 18), cols_B_to = NULL),
  list(sheet = 69, rows_A = c(6, 8, 10, 12, 14, 16, 18), rows_B = c(7, 9, 11, 13, 15, 17, 19), cols_B_to = "2:16"),
  list(sheet = 85, rows_A = c(6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34), rows_B = c(7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35), cols_B_to = "B:L"),
  list(sheet = 113, rows_A = c(8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50, 53, 56, 59, 62, 65, 68, 71, 74, 77, 80, 83, 86, 89, 92, 95, 98, 101, 104, 107, 110, 113, 116, 119), 
       rows_B = c(9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 78, 81, 84, 87, 90, 93, 96, 99, 102, 105, 108, 111, 114, 117, 120), cols_B_to = NULL),
  list(sheet = 114, rows_A = c(8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50), rows_B = c(9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51), cols_B_to = NULL)
)

#### Save the Consolidated Data ####
write.csv(combined_data, "data/02-analysis_data/twenty_twenty_two_summary_analysis_data.csv")





#### Flexible Function for Data Extraction ####
extract_data <- function(sheet_index, rows_A, rows_B, cols_B_to) {
  # Read the sheet
  data <- read.xlsx(file_path, sheet = sheet_index, colNames = FALSE)
  
  # Select rows specified in rows_A and rows_B
  data_A <- data[rows_A, , drop = FALSE]
  data_B <- data[rows_B, , drop = FALSE]
  
  # Clean Column A (Category) for rows_A
  data_A <- data_A %>%
    rename(Category = X1) %>%
    mutate(across(-Category, ~ gsub("-", "0", .))) %>%
    mutate(across(
      -Category,
      ~ ifelse(grepl("^[0-9.]+$", .), as.numeric(.), NA_real_)
    ))
  
  # Clean Columns B-to-end for rows_B
  if (!is.null(cols_B_to)) {
    data_B <- data_B %>%
      select(1, cols_B_to) %>%
      mutate(across(-1, ~ gsub("-", "0", .))) %>%
      mutate(across(
        -1,
        ~ ifelse(grepl("^[0-9.]+$", .), as.numeric(.), NA_real_)
      ))
  } else {
    data_B <- data_B %>%
      mutate(across(-1, ~ gsub("-", "0", .))) %>%
      mutate(across(
        -1,
        ~ ifelse(grepl("^[0-9.]+$", .), as.numeric(.), NA_real_)
      ))
  }
  
  # Combine Data A and Data B
  combined_data <- bind_rows(data_A, data_B)
  return(combined_data)
}

#### Loop Through All Sheets ####
combined_data <- lapply(sheet_params, function(params) {
  extract_data(
    sheet_index = params$sheet,
    rows_A = params$rows_A,
    rows_B = params$rows_B,
    cols_B_to = params$cols_B_to
  )
}) %>% bind_rows()

