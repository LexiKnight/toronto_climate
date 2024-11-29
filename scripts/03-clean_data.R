#### Preamble ####
# Purpose: Cleans the raw climate perceptions data recorded to prepare it for analysis.
# Author: Lexi Knight
# Date: 10 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete 01-download_data.R in scripts folder in order to access raw data.


#### Workspace setup ####
# install necessary packages
# install.packages(c("readr", "dplyr", "openxlsx", "readxl", "arrow", "tidyverse", "tidyr"))

# load necessary packages
library(readr)
library(dplyr) # for combining data frames
library(openxlsx)
library(readxl)
library(arrow) # for saving file as parquet
library(tidyverse)
library(tidyr)


#### Clean individual 2018 data ####

# Read in the csv file
twenty_eighteen_raw <- read_csv("data/01-raw_data/twenty_eighteen_raw_data.csv")

# Sample size information
sample_size_2018 <- 404 # This is the sample size for the year 2018

#  Select certain columns outlined below
twenty_eighteen <- twenty_eighteen_raw %>%
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

# Rename columns for clarity and meaning via rename() function
twenty_eighteen <- twenty_eighteen %>%
  rename(
    age = HIDAGE1,
    extent_consider_informed = Q2,
    likelihood_action_home_improvement= Q10r1,
    likelihood_action_reduce_hydro = Q10r2,
    likelihood_action_minimize_car = Q10r3,
    likelihood_action_vehicle_electric = Q10r4,
    likelihood_action_protein_alternative = Q10r5,
    likelihood_action_reduce_waste = Q10r6,
    likelihood_action_green_product = Q10r7,
    likelihood_action_short_distance = Q10r8,
    likelihood_action_sort_waste = Q10r9,
    unlikelihood_action_home_improvement_confusing = Q11_Lr1r1,
    unlikelihood_action_home_improvement_individual_difference = Q11_Lr1r2,
    unlikelihood_action_home_improvement_ineffective = Q11_Lr1r3,
    unlikelihood_action_home_improvement_costly = Q11_Lr1r4,
    unlikelihood_action_home_improvement_unavailable = Q11_Lr1r5,
    unlikelihood_action_home_improvement_inconvenient = Q11_Lr1r6,
    unlikelihood_action_home_improvement_uninterested = Q11_Lr1r7,
    unlikelihood_action_home_improvement_other = Q11_Lr1r8,
    unlikelihood_action_reduce_hydro_confusing = Q11_Lr2r1,
    unlikelihood_action_reduce_hydro_individual_difference = Q11_Lr2r2,
    unlikelihood_action_reduce_hydro_ineffective = Q11_Lr2r3,
    unlikelihood_action_reduce_hydro_costly = Q11_Lr2r4,
    unlikelihood_action_reduce_hydro_unavailable = Q11_Lr2r5,
    unlikelihood_action_reduce_hydro_inconvenient = Q11_Lr2r6,
    unlikelihood_action_reduce_hydro_uninterested = Q11_Lr2r7,
    unlikelihood_action_reduce_hydro_other = Q11_Lr2r8,
    unlikelihood_action_minimize_car_confusing = Q11_Lr3r1,
    unlikelihood_action_minimize_car_individual_difference = Q11_Lr3r2,
    unlikelihood_action_minimize_car_ineffective = Q11_Lr3r3,
    unlikelihood_action_minimize_car_costly = Q11_Lr3r4,
    unlikelihood_action_minimize_car_unavailable = Q11_Lr3r5,
    unlikelihood_action_minimize_car_inconvenient = Q11_Lr3r6,
    unlikelihood_action_minimize_car_uninterested = Q11_Lr3r7,
    unlikelihood_action_minimize_car_other = Q11_Lr3r8,
    unlikelihood_action_vehicle_electric_confusing = Q11_Lr4r1,
    unlikelihood_action_vehicle_electric_individual_difference = Q11_Lr4r2,
    unlikelihood_action_vehicle_electric_ineffective = Q11_Lr4r3,
    unlikelihood_action_vehicle_electric_costly = Q11_Lr4r4,
    unlikelihood_action_vehicle_electric_unavailable = Q11_Lr4r5,
    unlikelihood_action_vehicle_electric_inconvenient = Q11_Lr4r6,
    unlikelihood_action_vehicle_electric_uninterested = Q11_Lr4r7,
    unlikelihood_action_vehicle_electric_other = Q11_Lr4r8,
    unlikelihood_action_protein_alternative_confusing = Q11_Lr5r1,
    unlikelihood_action_protein_alternative_individual_difference = Q11_Lr5r2,
    unlikelihood_action_protein_alternative_ineffective = Q11_Lr5r3,
    unlikelihood_action_protein_alternative_costly = Q11_Lr5r4,
    unlikelihood_action_protein_alternative_unavailable = Q11_Lr5r5,
    unlikelihood_action_protein_alternative_inconvenient = Q11_Lr5r6,
    unlikelihood_action_protein_alternative_uninterested = Q11_Lr5r7,
    unlikelihood_action_protein_alternative_other = Q11_Lr5r8,
    unlikelihood_action_reduce_waste_confusing = Q11_Lr6r1,
    unlikelihood_action_reduce_waste_individual_difference = Q11_Lr6r2,
    unlikelihood_action_reduce_waste_ineffective = Q11_Lr6r3,
    unlikelihood_action_reduce_waste_costly = Q11_Lr6r4,
    unlikelihood_action_reduce_waste_unavailable = Q11_Lr6r5,
    unlikelihood_action_reduce_waste_inconvenient = Q11_Lr6r6,
    unlikelihood_action_reduce_waste_uninterested = Q11_Lr6r7,
    unlikelihood_action_reduce_waste_other = Q11_Lr6r8,
    unlikelihood_action_green_product_confusing = Q11_Lr7r1,
    unlikelihood_action_green_product_individual_difference = Q11_Lr7r2,
    unlikelihood_action_green_product_ineffective = Q11_Lr7r3,
    unlikelihood_action_green_product_costly = Q11_Lr7r4,
    unlikelihood_action_green_product_unavailable = Q11_Lr7r5,
    unlikelihood_action_green_product_inconvenient = Q11_Lr7r6,
    unlikelihood_action_green_product_uninterested = Q11_Lr7r7,
    unlikelihood_action_green_product_other = Q11_Lr7r8,
    unlikelihood_action_short_distance_confusing = Q11_Lr8r1,
    unlikelihood_action_short_distance_individual_difference = Q11_Lr8r2,
    unlikelihood_action_short_distance_ineffective = Q11_Lr8r3,
    unlikelihood_action_short_distance_costly = Q11_Lr8r4,
    unlikelihood_action_short_distance_unavailable = Q11_Lr8r5,
    unlikelihood_action_short_distance_inconvenient = Q11_Lr8r6,
    unlikelihood_action_short_distance_uninterested = Q11_Lr8r7,
    unlikelihood_action_short_distance_other = Q11_Lr8r8,
    unlikelihood_action_sort_waste_confusing = Q11_Lr9r1,
    unlikelihood_action_sort_waste_individual_difference = Q11_Lr9r2,
    unlikelihood_action_sort_waste_ineffective = Q11_Lr9r3,
    unlikelihood_action_sort_waste_costly = Q11_Lr9r4,
    unlikelihood_action_sort_waste_unavailable = Q11_Lr9r5,
    unlikelihood_action_sort_waste_inconvenient = Q11_Lr9r6,
    unlikelihood_action_sort_waste_uninterested = Q11_Lr9r7,
    unlikelihood_action_sort_waste_other = Q11_Lr9r8,
    delivery_method_toronto.ca_website = Q13r1,
    delivery_method_events = Q13r2,
    delivery_method_twitter = Q13r3,
    delivery_method_facebook = Q13r4,
    delivery_method_instagram = Q13r5,
    delivery_method_enewsletter_email = Q13r6,
    delivery_method_councillor_communication = Q13r7,
    delivery_method_advertising_campaigns = Q13r8,
    delivery_method_brochures_pamphlets = Q13r9,
    delivery_method_other = Q13r10,
    delivery_method_not_interested_receiving = Q13r11,
    highest_level_educ = QD5
  )
# Change "NA" values to "No answer" in the relevant columns
twenty_eighteen <- twenty_eighteen %>%
  mutate(across(
    c(likelihood_action_home_improvement, 
      likelihood_action_reduce_hydro, 
      likelihood_action_minimize_car, 
      likelihood_action_vehicle_electric, 
      likelihood_action_protein_alternative, 
      likelihood_action_reduce_waste, 
      likelihood_action_green_product, 
      likelihood_action_short_distance, 
      likelihood_action_sort_waste), 
    ~ replace_na(., "No answer")
  ))

twenty_eighteen <- twenty_eighteen %>%
  mutate(
    across(
      starts_with("unlikelihood_action"),  # Apply to columns starting with 'unlikelihood_action'
      ~ ifelse(!is.na(.), "unlikely", .)  # Replace non-NA values with "unlikely", keep NA as is
    )
  )

twenty_eighteen <- twenty_eighteen %>%
  mutate(
    across(
      starts_with("delivery_method"),  # Apply to columns starting with 'delivery_method'
      ~ ifelse(str_starts(., "NO TO:"), "no", .)  # Replace "NO TO:" with "no", leave other values unchanged
    )
  ) %>%
  mutate(
    delivery_method_events = ifelse(delivery_method_events != "no", "events", "no"),
    delivery_method_enewsletter_email = ifelse(delivery_method_enewsletter_email != "no", "enewsletter/email", "no"),
    delivery_method_advertising_campaigns = ifelse(delivery_method_advertising_campaigns != "no", "advertising campaigns", "no"),
    delivery_method_brochures_pamphlets = ifelse(delivery_method_brochures_pamphlets != "no", "brochures/pamphlets", "no"),
    delivery_method_other = ifelse(delivery_method_other != "no", "other", "no")
  )


# View the first few rows to confirm the data
head(twenty_eighteen)

#### Save 2018 individual data ####
write_parquet(twenty_eighteen, "data/02-analysis_data/twenty_eighteen_individual_analysis_data.parquet")
write_csv(twenty_eighteen, "data/02-analysis_data/twenty_eighteen_individual_analysis_data.csv")










#### Clean data summary 2018 dataset ####

# Read the csv file
twenty_eighteen <- read_csv("data/02-analysis_data/twenty_eighteen_individual_analysis_data.csv", col_names = TRUE)


## 1. Age Summary ##

## Create age categories based on ranges in 2021 summary data and summarize directly
age_summary_18 <- as.data.frame(table(cut(twenty_eighteen$age, 
                                         breaks = c(17, 23, 39, 55, Inf), 
                                         labels = c("18-23", "24-39", "40-55", "56+"), 
                                         right = TRUE)))

# Rename columns and calculate the percentage
names(age_summary_18) <- c("Age Group", "Freq")
age_summary_18$Percentage <- round((age_summary_18$Freq / sum(age_summary_18$Freq)) * 100)

# Keep only the age group and percentage columns
age_summary_18 <- age_summary_18[, c("Age Group", "Percentage")]

# View the final summary table
print(age_summary_18)

# save as parquet
write_parquet(age_summary_18, "data/02-analysis_data/twenty_eighteen_individual_analysis_data.parquet")



## 2. Education Level Summary ##

# Create the summary table based on education levels
education_summary_18 <- as.data.frame(table(twenty_eighteen$highest_level_educ))

# Rename columns and calculate the percentage
names(education_summary_18) <- c("Education Level", "Freq")
education_summary_18$Percentage <- round((education_summary_18$Freq / sum(education_summary_18$Freq)) * 100)

# Keep only the education level and percentage columns
education_summary_18 <- education_summary_18[, c("Education Level", "Percentage")]

# View the final education summary table
print(education_summary_18)

# save as parquet
write_parquet(education_summary_18, "data/02-analysis_data/twenty_eighteen_individual_analysis_data.parquet")



## 3. Informed Summary ##

# Create the summary table based on the informed extent categories
informed_summary_18 <- as.data.frame(table(twenty_eighteen$extent_consider_informed))

# Rename columns and calculate the percentage
names(informed_summary_18) <- c("Informed Level", "Freq")
informed_summary_18$Percentage <- round((informed_summary_18$Freq / sum(informed_summary_18$Freq)) * 100)

# Keep only the informed level and percentage columns
informed_summary_18 <- informed_summary_18[, c("Informed Level", "Percentage")]

# View the final informed summary table
print(informed_summary_18)

# save as parquet
write_parquet(informed_summary_18, "data/02-analysis_data/twenty_eighteen_individual_analysis_data.parquet")





## 4. Likelihood Action Summary ##

# Identify columns starting with "likelihood_action"
likelihood_columns <- grep("^likelihood_action", names(twenty_eighteen), value = TRUE)

# Subset only the first 9 columns/actions (based on the actions you specified)
likelihood_columns <- likelihood_columns[1:9]

# Define the new names for the columns based on the actions you described
action_names <- c(
  "Home Improvement",
  "Reduce Hydro Use",
  "Minimize Car Use",
  "Electric / Hybrid Vehicle",
  "Meat Alternative",
  "Reduce Waste",
  "Purchase Green Product",
  "Walk / Cycle Short Distance",
  "Sort Waste Correctly"
)

# Create the summary table based on the likelihood action categories and convert to percentages
likelihood_summary_18 <- lapply(likelihood_columns, function(col) {
  counts <- table(twenty_eighteen[[col]])
  percentages <- round(100 * counts / sum(counts))  # Convert counts to percentages, round to whole numbers
  return(percentages)
})

# Convert the list of tables into a data frame
likelihood_summary_18 <- as.data.frame(do.call(cbind, likelihood_summary_18))

# Add "Likelihood" as a new first column with factor labels (extracting from row names)
likelihood_summary_18 <- cbind(Likelihood = rownames(likelihood_summary_18), likelihood_summary_18)

# Rename "Already doing this or have done this" to "Doing / have done"
likelihood_summary_18$Likelihood <- sub("Already doing this or have done this", "Doing / have done", likelihood_summary_18$Likelihood)

# Assign correct column names (Likelihood + action names) to the data frame
colnames(likelihood_summary_18) <- c("Likelihood", action_names)

# Ensure row names are not mistakenly treated as data, and remove row names for clean export
rownames(likelihood_summary_18) <- NULL

# Print the cleaned data frame to verify the structure
print(likelihood_summary_18)

# Save as Parquet File (Ensure data is written as a data frame, not matrix)
write_parquet(likelihood_summary_18, "data/02-analysis_data/twenty_eighteen_individual_analysis_data.parquet")



## 5. Reasons Summary ##

# Identify columns starting with "unlikelihood_action"
action_columns <- grep("^unlikelihood_action", names(twenty_eighteen), value = TRUE)

# Define all possible reasons
reasons <- c("Confusing", "Costly", "Inconvenient", "Individual Difference", 
             "Ineffective", "Other", "Unavailable", "Uninterested")

# Initialize an empty data frame to store results
reason_summary_18 <- data.frame()

# Define the action categories (the names of the columns you're interested in)
action_categories <- c(
  "Home Improvement", 
  "Reduce Hydro Use", 
  "Electric / Hybrid Car", 
  "Meat Alternative", 
  "Reduce Waste", 
  "Purchase Green Products", 
  "Walk / Cycle Short Distance", 
  "Sort Waste Correctly"
)

# Loop through each action column and count the number of "unlikely" occurrences
for (action in action_columns) {
  # Get the reason from the column name by removing the prefix and reason part
  reason_part <- gsub("^unlikelihood_action_", "", action)
  reason_part <- gsub(paste(reasons, collapse = "|"), "", reason_part)  # Remove reason from the action name
  
  # Count the number of "unlikely" occurrences in the current action column
  action_data <- twenty_eighteen[[action]]
  unlikely_count <- sum(tolower(trimws(action_data)) == "unlikely", na.rm = TRUE)
  
  # Add the count to a data frame with the reason (the cleaned action name) and the corresponding action
  action_df <- data.frame(
    Reason = reason_part,  # Renaming "unlikely" with the cleaned action name
    Count = unlikely_count,
    Action = gsub("unlikelihood_action_", "", action)  # Clean the action name
  )
  
  # Add the action-specific data to the combined summary
  reason_summary_18 <- rbind(reason_summary_18, action_df)
}

# Pivot the table to create a wide-format table
reason_summary_18 <- reason_summary_18 %>%
  pivot_wider(names_from = Action, values_from = Count, values_fill = list(Count = 0))

# Rename columns to match the desired action categories
colnames(reason_summary_18)[-1] <- action_categories

# Print the final table
print(reason_summary_18)

# Save as Parquet File (optional)
write_parquet(reason_summary_18, "data/02-analysis_data/twenty_eighteen_reasons_summary.parquet")




#### 6. Communication Summary ####

# Filter columns that start with "delivery_method"
communication_columns <- twenty_eighteen %>%
  select(starts_with("delivery_method"))

# Step 2: Convert to long format to aggregate all delivery methods
communication_summary_18 <- communication_columns %>%
  pivot_longer(cols = everything(), names_to = "Communication Method", values_to = "Response") %>%
  group_by(`Communication Method`, Response) %>%
  summarise(Freq = n(), .groups = "drop") %>%
  mutate(Total = sum(Freq), .by = `Communication Method`) %>%
  mutate(Percentage = round((Freq / Total) * 100))

# Step 3: Keep only the "Yes" responses and rename the "Yes (%)" column
communication_summary_18 <- communication_summary_18 %>%
  filter(Response == "yes") %>%
  select(`Communication Method`, Percentage)

# Step 4: Rename the percentage column to "Percentage"
colnames(communication_summary_18)[2] <- "Percentage"

# Step 5: View the final communication summary table
print(communication_summary_18)











#### Clean data 2021 ####

# File path to the saved workbook
file_path <- "data/01-raw_data/twenty_twenty_one_raw_data.xlsx"

# Sample size information
sample_size_2021 <- 1401 # This is the sample size for the year 2021

## Age - sheet 3 ##

# Extract data from Sheet 3 + 1 (accounting for the index sheet)
sheet4_data <- read_excel(file_path, sheet = 4)

# Define rows and columns for Sheet 3 that want extract 
rows_A <- c(7, 10, 13, 16)  # Categories in column A (age categories)
rows_B <- c(8, 11, 14, 17)  # Data in column B

# Extract the age categories from column A
categories <- sheet4_data %>%
  slice(rows_A) %>%
  pull(1) %>%  # Pull the first column (A) as a vector
  trimws()  # Remove unexpected whitespace

# Extract the percentage raw data from column B 
raw_data_B <- sheet4_data[rows_B, 2]

# Convert raw data from tibble to character and then to numeric
raw_data_B_numeric <- as.numeric(as.character(raw_data_B[[1]]))

# Multiply by 100 to convert to percentages (if the conversion succeeded)
percentages <- round(raw_data_B_numeric * 100)

## Save the age_summary_21 ##

# Combine categories and percentages into a data frame
age_summary_21 <- data.frame(
  Category = categories,
  Percentage = percentages
)

# Rename the column headers
colnames(age_summary_21) <- c("Age Group", "Age Percentage")

# Print the consolidated data to check
print(age_summary_21)





## education - sheet 11 ##

# Extract data from Sheet 11 + 1 (accounting for the index sheet)
sheet12_data <- read_excel(file_path, sheet = 12)

# Define rows and columns for Sheet 11 that want extract 
rows_A <- c(7, 10, 13, 16, 19, 22)  # Categories in column A (education categories)
rows_B <- c(8, 11, 14, 17, 20, 23)  # Data in column B

# Extract the education categories from column A
categories <- sheet12_data %>%
  slice(rows_A) %>%
  pull(1) %>%  # Pull the first column (A) as a vector
  trimws()  # Remove unexpected whitespace

# Extract the percentage raw data from column B 
raw_data_B <- sheet12_data[rows_B, 2]

# Convert raw data from tibble to character and then to numeric
raw_data_B_numeric <- as.numeric(as.character(raw_data_B[[1]]))

# Multiply by 100 to convert to percentages (if the conversion succeeded)
percentages <- round(raw_data_B_numeric * 100)

## Save the education_summary_21 ##

# Combine categories and percentages into a data frame
education_summary_21 <- data.frame(
  Category = categories,
  Percentage = percentages
)

# Rename the column headers
colnames(education_summary_21) <- c("Education Level", "Education Percentage")

# Print the consolidated data to check
print(education_summary_21)





## Extent Informed - sheet 34 ##

# Extract data from Sheet 34 + 1 (accounting for the index sheet)
sheet35_data <- read_excel(file_path, sheet = 35)

# Define rows and columns for Sheet 11 that want extract 
rows_A <- c(7, 10, 13, 16)  # Categories in column A (extent informed categories)
rows_B <- c(8, 11, 14, 17)  # Data in column B

# Extract the extent informed categories from column A
categories <- sheet35_data %>%
  slice(rows_A) %>%
  pull(1) %>%  # Pull the first column (A) as a vector
  trimws()  # Remove unexpected whitespace

# Extract the percentage raw data from column B 
raw_data_B <- sheet35_data[rows_B, 2]

# Convert raw data from tibble to character and then to numeric
raw_data_B_numeric <- as.numeric(as.character(raw_data_B[[1]]))

# Multiply by 100 to convert to percentages (if the conversion succeeded)
percentages <- round(raw_data_B_numeric * 100)

## Save the informed_summary_21 ##

# Combine categories and percentages into a data frame
informed_summary_21 <- data.frame(
  Category = categories,
  Percentage = percentages
)

# Rename the column headers
colnames(informed_summary_21) <- c("Extent Informed", "Informed Percentage")

# Print the consolidated data to check
print(informed_summary_21)





## Likelihood Take Action - sheet 69 ##

# Extract data from Sheet 69 + 1 (accounting for the index sheet)
sheet70_data <- read_excel(file_path, sheet = 70)

# Define rows and columns for Sheet 11 that want extract 
rows_A <- c(5, 7, 9, 11, 13, 15, 17)  # Categories in column A (likelihood categories)
rows_B_to_P <- c(6, 8, 10, 12, 14, 16, 18)  # Rows for data in columns B through P

# Extract the likelihood take action categories from column A
categories <- sheet70_data %>%
  slice(rows_A) %>%
  pull(1) %>%  # Pull the first column (A) as a vector
  trimws()  # Remove unexpected whitespace

# Rename the 13th category (index 5 in rows_A) to "Already doing/done in past year"
categories[5] <- "Already doing/done in past year"
# Rename the 17th category (index 7 in rows_A) to "Unsure"
categories[7] <- "Unsure"

# Initialize a list to store data for columns B through P
percentage_list <- list()

# Loop through columns B to P and extract corresponding data
for (col_index in 2:16) {  # Columns B (2) to P (16) in R indexing
  raw_data <- sheet70_data[rows_B_to_P, col_index]
  
  # Convert raw data to character, replace "-" with "0", then convert to numeric
  raw_data_cleaned <- as.character(raw_data[[1]])
  raw_data_cleaned[raw_data_cleaned == "-"] <- "0"
  
  # Convert the cleaned data to numeric and multiply by 100 (if conversion succeeded)
  numeric_data <- round(as.numeric(raw_data_cleaned) * 100)
  percentage_list[[paste0("Col_", LETTERS[col_index])]] <- numeric_data
}

# Ensure values are rounded to whole numbers and formatted correctly
percentages <- format(percentages, nsmall = 0)  # No decimal places

# Combine categories and percentages into a data frame
likelihood_summary_21 <- data.frame(
  Category = categories,
  percentage_list
)

colnames(likelihood_summary_21) <- c(
  "Likelihood to Take Action",
  "Likely purchase energy efficient appliances",
  "Likely install a programmable thermostat",
  "Likely install LED lightbulbs",
  "Likely undertake major home renos for energy efficiency",
  "Likely add solar panels to home",
  "Likely get an EnerGuide home energy evaluation to identify opportunities",
  "Likely reduce water use",
  "Likely use public transit more",
  "Likely cycle more",
  "Likely walk more",
  "Likely purchase electric/hybrid vehicle in next 1-3 years",
  "Likely eat less meat",
  "Likely reduce amount of own waste",
  "Likely purchase environmentally friendly items",
  "Likely put effort into sorting waste into correct bins"
)

# Print the consolidated data to check
print("Likelihood Summary:")
print(likelihood_summary_21)





## Reason Unlikely to Take Action - sheet 85 ##

# Extract data from Sheet 85 + 1 (accounting for the index sheet)
sheet86_data <- read_excel(file_path, sheet = 86)

# Define rows and columns for Sheet 11 that want extract 
rows_A <- c(5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33)  # Categories in column A (reason categories)
rows_B_to_P <- c(6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34)  # Rows for data in columns B through P

# Extract the likelihood take action categories from column A
categories <- sheet86_data %>%
  slice(rows_A) %>%
  pull(1) %>%  # Pull the first column (A) as a vector
  trimws()  # Remove unexpected whitespace

# Initialize a list to store data for columns B through L
percentage_list <- list()

# Loop through columns B to L and extract corresponding data
for (col_index in 2:12) {  # Columns B (2) to L (12) in R indexing
  raw_data <- sheet86_data[rows_B_to_P, col_index]
  
  # Convert raw data to character, replace "-" with "0", then convert to numeric
  raw_data_cleaned <- as.character(raw_data[[1]])
  raw_data_cleaned[raw_data_cleaned == "-"] <- "0"
  
  # Convert the cleaned data to numeric and multiply by 100 (if conversion succeeded)
  numeric_data <- round(as.numeric(raw_data_cleaned) * 100)
  percentage_list[[paste0("Col_", LETTERS[col_index])]] <- numeric_data
}

# Ensure values are rounded to whole numbers and formatted correctly
percentages <- format(percentages, nsmall = 0)  # No decimal places

# Combine categories and percentages into a data frame
reasons_summary_21 <- data.frame(
  Category = categories,
  percentage_list
)

colnames(reasons_summary_21) <- c(
  "Reasons unlikely to take action",
  "Unlikely purchase energy efficient appliances",
  "Unlikely install a programmable thermostat",
  "Unlikely install LED lightbulbs",
  "Unlikely undertake major home renos for energy efficiency",
  "Unlikely add solar panels to home",
  "Unlikely get an EnerGuide home energy evaluation to identify opportunities",
  "Unlikely reduce water use",
  "Unlikely eat less meat",
  "Unlikely reduce amount of own waste",
  "Unlikely purchase environmentally friendly items",
  "Unlikely put effort into sorting waste into correct bins"
)

# Print the consolidated data to check
print("Reasons Summary:")
print(reasons_summary_21)





## City support - sheet 113 ##

# Extract data from Sheet 113 + 1 (accounting for the index sheet)
sheet114_data <- read_excel(file_path, sheet = 114)

# Define rows and columns for Sheet 113 that want extract 
rows_A <- c(7, 10, 13, 16, 19,22, 25, 28, 31,34, 37,40, 43, 46, 49, 52, 55, 58,
            61, 64, 67, 70, 73, 76, 79, 82, 85, 88, 91, 94, 97, 100, 103, 106,
            109, 112,115, 118) # Categories in column A (city support categories)
rows_B <- c(8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50, 53, 56, 59,
           62, 65, 68, 71, 74, 77, 80, 83, 86, 89, 92, 95, 98, 101, 104, 107,
           110, 113, 116, 119) # Data in column B

# Extract the city support categories from column A
categories <- sheet114_data %>%
  slice(rows_A) %>%
  pull(1) %>%  # Pull the first column (A) as a vector
  trimws()  # Remove unexpected whitespace

# Extract the percentage raw data from column B 
raw_data_B <- sheet114_data[rows_B, 2]

# Convert raw data from tibble to character and then to numeric
raw_data_B_numeric <- as.numeric(as.character(raw_data_B[[1]]))

# Multiply by 100 to convert to percentages (if the conversion succeeded)
percentages <- round(raw_data_B_numeric * 100)


## Save the informed_summary_21 ##

# Combine categories and percentages into a data frame
support_summary_21 <- data.frame(
  Category = categories,
  Percentage = percentages
)

# Rename the column headers
colnames(support_summary_21) <- c("City Support to Motivate", "Support Percentage")

# Print the consolidated data to check
print(support_summary_21)






## Methods of communication - sheet 114 ##

# Extract data from Sheet 113 + 1 (accounting for the index sheet)
sheet115_data <- read_excel(file_path, sheet = 115)

# Define rows and columns for Sheet 113 that want extract 
rows_A <- c(7, 10, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46, 49)
# Categories in column A (methods of communication)
rows_B <- c(8, 11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 41, 44, 47, 50) 
# Data in column B

# Extract the city support categories from column A
categories <- sheet115_data %>%
  slice(rows_A) %>%
  pull(1) %>%  # Pull the first column (A) as a vector
  trimws()  # Remove unexpected whitespace

# Rename the 8th category (index 8 in rows_A)
categories[8] <- "Advertising campaigns"

# Extract the percentage raw data from column B 
raw_data_B <- sheet115_data[rows_B, 2]

# Convert raw data from tibble to character and then to numeric
raw_data_B_numeric <- as.numeric(as.character(raw_data_B[[1]]))

# Multiply by 100 to convert to percentages (if the conversion succeeded)
percentages <- round(raw_data_B_numeric * 100)

## Save the informed_summary_21 ##

# Combine categories and percentages into a data frame
communication_summary_21 <- data.frame(
  Category = categories,
  Percentage = percentages
)

# Rename the column headers
colnames(communication_summary_21) <- c("Method of Communication", "Communication Percentage")

# Print the consolidated data to check
print(communication_summary_21)





## Combine Individual 2021 Data ##
# Find the maximum number of rows in the datasets to ensure they're aligned
num_rows <- sapply(list(
  age_summary_21,
  education_summary_21,
  informed_summary_21,
  likelihood_summary_21,
  reasons_summary_21,
  support_summary_21,
  communication_summary_21
), nrow)

max_rows <- max(num_rows)  # Find the maximum number of rows
print(num_rows)  # Print number of rows for debugging

# Function to pad data frames with NA if they have fewer rows than the max number
pad_with_na <- function(df, max_rows) {
  if (nrow(df) < max_rows) {
    # Pad the data frame with NA rows
    pad_rows <- max_rows - nrow(df)
    df <- bind_rows(df, as.data.frame(matrix(NA, nrow = pad_rows, ncol = ncol(df))))
  }
  return(df)
}

# Pad each data frame
age_summary_21 <- pad_with_na(age_summary_21, max_rows)
education_summary_21 <- pad_with_na(education_summary_21, max_rows)
informed_summary_21 <- pad_with_na(informed_summary_21, max_rows)
likelihood_summary_21 <- pad_with_na(likelihood_summary_21, max_rows)
reasons_summary_21 <- pad_with_na(reasons_summary_21, max_rows)
support_summary_21 <- pad_with_na(support_summary_21, max_rows)
communication_summary_21 <- pad_with_na(communication_summary_21, max_rows)

# Combine datasets side by side
combined_data <- bind_cols(
  age_summary_21,
  education_summary_21,
  informed_summary_21,
  likelihood_summary_21,
  reasons_summary_21,
  support_summary_21,
  communication_summary_21
)

# Clean the data by removing columns that start with "V"
data_cleaned <- combined_data %>%
  select(-starts_with("V"))  # Removes columns starting with "V"

# Save the combined data as a Parquet file and csv file
write_parquet(data_cleaned, "data/02-analysis_data/twenty_twenty_two_summary_analysis_data.parquet")
write_csv(data_cleaned, "data/02-analysis_data/twenty_twenty_two_summary_analysis_data.csv")

# Confirmation message
print("Data saved successfully!")

