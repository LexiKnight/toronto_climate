#### Preamble ####
# Purpose: Cleans the raw climate perceptions data recorded to prepare it for analysis.
# Author: Lexi Knight
# Date: 10 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete 01-download_data.R in scripts folder in order to access raw data.


#### Workspace setup ####

# load necessary packages
library(readr)       # reads csv files
library(dplyr)       # data manipulation; selecting, renaming, mutating columns
library(arrow)       # saves data as parquet
library(stringr)     # string manipulation
library(here)        # constructs file paths
library(tinytable)   # creates and formats tables

#### Clean individual 2018 data ####

# Read in the csv file
twenty_eighteen_raw <- read_csv("data/01-raw_data/twenty_eighteen_raw_data.csv")

# Sample size information
sample_size_2018 <- 404 # This is the sample size for the year 2018

#  Select certain columns outlined below
twenty_eighteen <- twenty_eighteen_raw %>%
  select(HIDAGE1, Q2, Q10r1, Q10r2, Q10r3, Q10r4, Q10r5, Q10r6, Q10r7, Q10r8,
         Q10r9, Q13r1, Q13r2, Q13r3, Q13r4, Q13r5, Q13r6, Q13r7, Q13r8, Q13r9,
         Q13r10, Q13r11,QD5)

# Rename columns for clarity and meaning via rename() function
twenty_eighteen <- twenty_eighteen %>%
  rename(
    age = HIDAGE1,
    extent_consider_informed = Q2,
    likelihood_home_improvement= Q10r1,
    likelihood_reduce_hydro = Q10r2,
    likelihood_minimize_car = Q10r3,
    likelihood_vehicle_electric = Q10r4,
    likelihood_protein_alternative = Q10r5,
    likelihood_reduce_waste = Q10r6,
    likelihood_green_product = Q10r7,
    likelihood_short_distance = Q10r8,
    likelihood_sort_waste = Q10r9,
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

# Check after cleaning "likelihood" columns
twenty_eighteen <- twenty_eighteen %>%
  mutate(across(
    starts_with("likelihood"),
    ~ gsub("Verylikely", "Very likely", .)
  ))


# Handle "unlikelihood" columns
unlikelihood_columns <- grep("^unlikelihood", names(twenty_eighteen), value = TRUE)

for (col in unlikelihood_columns) {
  twenty_eighteen[[col]] <- sapply(twenty_eighteen[[col]], function(x) {
    if (is.na(x)) {
      return(0)
    } else {
      return(1)
    }
  })
}

# Handle "delivery_method" columns
delivery_method_columns <- grep("^delivery_method", names(twenty_eighteen), value = TRUE)

# Mapping for renaming specific non-"no" values
delivery_method_rename_map <- list(
  "toronto.ca_website" = "toronto.ca website",
  "events" = "events",
  "twitter" = "twitter",
  "facebook" = "facebook",
  "enewsletter_email" = "enewsletter / email",
  "councillor_communication" = "councillor communication",
  "advertising_campaigns" = "advertising campaigns",
  "brochures_pamphlets" = "brochures / pamphlets",
  "other" = "other",
  "not_interested_receiving" = "not interested"
)

# Loop through each "delivery_method" column
for (col in delivery_method_columns) {
  # Remove the prefix 'delivery_method_' from the column name
  col_name <- sub("delivery_method_", "", col)
  
  twenty_eighteen[[col]] <- sapply(twenty_eighteen[[col]], function(x) {
    if (grepl("^NO TO", x)) {
      return("no")
    } else {
      # Replace non-"no" values with the column name without the prefix
      return(col_name)
    }
  })
  
  # Now, rename specific values in the column using the predefined map
  twenty_eighteen[[col]] <- sapply(twenty_eighteen[[col]], function(x) {
    return(delivery_method_rename_map[[x]] %||% x)  # Use the mapped value if it exists, otherwise keep the original
  })
}

# View the first few rows to confirm the data
head(twenty_eighteen)

#### Save 2018 individual data ####
write_parquet(twenty_eighteen, "data/02-analysis_data/twenty_eighteen_individual_analysis_data.parquet")
write_csv(twenty_eighteen, "data/02-analysis_data/twenty_eighteen_individual_analysis_data.csv")






#### Clean data summary 2018 dataset ####

# Read the csv file
twenty_eighteen <- read_csv("data/02-analysis_data/twenty_eighteen_individual_analysis_data.csv", col_names = TRUE)


## 1. Age Summary 2018 ##

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

# Create the table using tinytable
age_summary_18_table <- tt(age_summary_18)

# print table
age_summary_18_table

# Save as Parquet
write_parquet(age_summary_18, here("data/03-figures_data", "age_summary_2018_table.parquet"))


## 2. Education Level Summary 2018 ##

# Create the summary table based on education levels
education_summary_18 <- as.data.frame(table(twenty_eighteen$highest_level_educ))

# Rename columns and calculate the percentage
names(education_summary_18) <- c("Education Level", "Freq")
education_summary_18$Percentage <- round((education_summary_18$Freq / sum(education_summary_18$Freq)) * 100)

# Keep only the education level and percentage columns
education_summary_18 <- education_summary_18[, c("Education Level", "Percentage")]

# Create the table using tinytable
education_summary_18_table <- tt(education_summary_18)

# Print table
education_summary_18_table

# Save as Parquet
write_parquet(education_summary_18, here("data/03-figures_data", "education_summary_2018_table.parquet"))



## 3. Informed Summary 2018 ##

# Create the summary table based on the informed extent categories
informed_summary_18 <- as.data.frame(table(twenty_eighteen$extent_consider_informed))

# Rename columns and calculate the percentage
names(informed_summary_18) <- c("Extent Informed", "Freq")
informed_summary_18$Percentage <- round((informed_summary_18$Freq / sum(informed_summary_18$Freq)) * 100)

# Keep only the informed level and percentage columns
informed_summary_18 <- informed_summary_18[, c("Extent Informed", "Percentage")]

# Create the table using tinytable
informed_summary_18_table <- tt(informed_summary_18)

# Print the styled table
informed_summary_18_table

# Save as Parquet
write_parquet(informed_summary_18, here("data/03-figures_data", "informed_summary_2018_table.parquet"))




## 4. Likelihood Action Summary 2018 ##

# Identify columns starting with "likelihood_action"
likelihood_columns <- grep("^likelihood", names(twenty_eighteen), value = TRUE)

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

# Add "Likelihood to Take Action" as a new first column with factor labels (extracting from row names)
likelihood_summary_18 <- cbind(Likelihood_to_Take_Action = rownames(likelihood_summary_18), likelihood_summary_18)

# Rename "Already doing this or have done this" to "Doing / have done"
likelihood_summary_18$Likelihood_to_Take_Action <- sub("Already doing this or have done this", "Doing / have done", likelihood_summary_18$Likelihood_to_Take_Action)

# Assign correct column names (Likelihood to Take Action + action names) to the data frame
colnames(likelihood_summary_18) <- c("Likelihood to Take Action", action_names)

# Ensure row names are not mistakenly treated as data, and remove row names for clean export
rownames(likelihood_summary_18) <- NULL

# Create the table using tinytable
likelihood_summary_18_table <- tt(likelihood_summary_18)

# print table
likelihood_summary_18_table

# Save as Parquet File (Ensure data is written as a data frame, not matrix)
write_parquet(likelihood_summary_18, here("data/03-figures_data", "likelihood_summary_2018_table.parquet"))




## 5. Communication Summary 2018 ##

# Select all columns starting with "delivery_method"
communication_summary_18 <- twenty_eighteen %>%
  select(starts_with("delivery_method"))

# Define the renaming mapping
delivery_method_rename_map <- list(
  "delivery_method_toronto.ca_website" = "Toronto.ca website",
  "delivery_method_events" = "Events",
  "delivery_method_twitter" = "Twitter",
  "delivery_method_facebook" = "Facebook",
  "delivery_method_instagram" = "Instagram",
  "delivery_method_enewsletter_email" = "Enewsletter / email",
  "delivery_method_councillor_communication" = "Councillor communication",
  "delivery_method_advertising_campaigns" = "Advertising campaigns",
  "delivery_method_brochures_pamphlets" = "Brochures / Pamphlets",
  "delivery_method_other" = "Other",
  "delivery_method_not_interested_receiving" = "Not interested"
)

# Calculate percentage of non-"no" occurrences
communication_summary_18 <- communication_summary_18 %>%
  summarise(across(everything(), ~ sum(. != "no") / n() * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Communication Method", 
               values_to = "Percentage") %>%
  mutate(Percentage = round(Percentage)) %>%
  filter(`Communication Method` != "total_rows")  # Filter out any unwanted rows

# Apply renaming based on the mapping
communication_summary_18 <- communication_summary_18 %>%
  mutate(`Communication Method` = recode(`Communication Method`, !!!delivery_method_rename_map))

# Create the table using tinytable (similar to age and education examples)
communication_summary_18_table <- tinytable::tt(communication_summary_18, 
                                                row.names = FALSE, 
                                                col.names = c("Communication Method", "Percentage"), 
                                                escape = FALSE)
# Print the table
communication_summary_18_table

# Save the data as Parquet
write_parquet(communication_summary_18, here("data/03-figures_data", "communication_summary_2018_table.parquet"))









#### Clean data 2021 ####

# File path to the saved workbook
twenty_twenty_one <- "data/01-raw_data/twenty_twenty_one_raw_data.xlsx"

# Sample size information
sample_size_2021 <- 1401 # This is the sample size for the year 2021




## Age 2021 - sheet 3 ##

# Extract data from Sheet 3 + 1 (accounting for the index sheet)
sheet4_data <- read_excel(twenty_twenty_one, sheet = 4)

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
colnames(age_summary_21) <- c("Age Group", "Percentage")

# Create the table using tinytable
age_summary_2021_table <- tt(age_summary_21)

# Print the styled table
age_summary_2021_table

# Save the data as Parquet
write_parquet(age_summary_21, here("data/03-figures_data", "age_summary_2021_table.parquet"))




## education 2021 - sheet 11 ##

# Extract data from Sheet 11 + 1 (accounting for the index sheet)
sheet12_data <- read_excel(twenty_twenty_one, sheet = 12)

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
colnames(education_summary_21) <- c("Education Level", "Percentage")

# Create the table using tinytable
education_summary_2021_table <- tt(education_summary_21)

# Print table
education_summary_2021_table

# Save the data as Parquet
write_parquet(education_summary_21, here("data/03-figures_data", "education_summary_2021_table.parquet"))




## Extent Informed 2021 - sheet 34 ##

# Extract data from Sheet 34 + 1 (accounting for the index sheet)
sheet35_data <- read_excel(twenty_twenty_one, sheet = 35)

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
colnames(informed_summary_21) <- c("Extent Informed", "Percentage")

# Create the table using tinytable
informed_summary_2021_table <- tt(informed_summary_21)

# Print table
informed_summary_2021_table

# Save the data as Parquet
write_parquet(informed_summary_21, here("data/03-figures_data", "informed_summary_2021_table.parquet"))




## Likelihood Take Action 2021- sheet 69 ##

# Extract data from Sheet 69 + 1 (accounting for the index sheet)
sheet70_data <- read_excel(twenty_twenty_one, sheet = 70)

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

# Remove the row with "Unsure"
likelihood_summary_21 <- likelihood_summary_21 %>%
  filter(Category != "Unsure")

# Update column names
colnames(likelihood_summary_21) <- c(
  "Likelihood to Take Action",
  "Purchase energy efficient appliances",
  "Install a programmable thermostat",
  "Install LED lightbulbs",
  "Major home renovations",
  "Add solar panels to home",
  "Get EnerGuide home energy evaluation",
  "Reduce water use",
  "Use public transit more",
  "Cycle more",
  "Walk more",
  "Purchase electric/hybrid vehicle in next 1-3 years",
  "Reduce meat consumption",
  "Reduce own waste",
  "Purchase environmentally friendly items",
  "Sorting waste into correct bins"
)

# Create the table using tinytable
likelihood_summary_2021_table <- tt(likelihood_summary_21)

# Print table
likelihood_summary_2021_table

# Save the data as Parquet
write_parquet(likelihood_summary_21, here("data/03-figures_data", "likelihood_summary_2021_table.parquet"))





## Methods of communication 2021- sheet 114 ##

# Extract data from Sheet 113 + 1 (accounting for the index sheet)
sheet115_data <- read_excel(twenty_twenty_one, sheet = 115)

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
colnames(communication_summary_21) <- c("Communication Method", "Percentage")

# Create the table using tinytable
communication_summary_2021_table <- tt(communication_summary_21)

# Print table
communication_summary_2021_table

# Save the data as Parquet
write_parquet(communication_summary_21, here("data/03-figures_data", "communication_summary_2021_table.parquet"))




## Combine Individual 2021 Data ##
# Find the maximum number of rows in the datasets to ensure they're aligned
num_rows <- sapply(list(
  age_summary_21,
  education_summary_21,
  informed_summary_21,
  likelihood_summary_21,
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
communication_summary_21 <- pad_with_na(communication_summary_21, max_rows)

# Combine datasets side by side
combined_data <- bind_cols(
  age_summary_21,
  education_summary_21,
  informed_summary_21,
  likelihood_summary_21,
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

