#### Preamble ####
# Purpose: Downloads and saves the data for years 2018 and 2021 from 
# opendatatoronto
# Author: Lexi Knight
# Date: 6 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
# Install the necessary packages if needed
# install.packages(c("httr", "readxl", "tidyverse", "openxlsx"))

# Load required libraries
library(httr)
library(readxl)
library(tidyverse)
library(openxlsx)

#### Download data for 2018 ####
# Note, there are two versions where each version is formatted differently, 
# here we use version 1.
# URL to the XLSX file
xlsx_url <- "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/dfbcc8c9-a754-4cae-83c8-2f508d110ab2/resource/ccb833b2-41c8-41d5-a918-e52d4113b212/download/City%20of%20Toronto%20Climate%20Perceptions%202018%20v1.xlsx"

# Download the file to a temporary file using httr::GET
response <- GET(xlsx_url, write_disk(tf <- tempfile(fileext = ".xlsx")))

# Check if the response is successful (HTTP 200)
if (response$status_code != 200) {
  stop("Failed to download file: HTTP ", response$status_code)
}

# Read the downloaded XLSX file into R
twenty_eighteen_raw_data <- read_excel(tf)

# View the first few rows to confirm the data
head(twenty_eighteen_raw_data)

#### Save the dataset ####
# Save the dataset to a CSV file
write_csv(twenty_eighteen_raw_data, "data/01-raw_data/twenty_eighteen_raw_data.csv")



#### Download data for 2021 ####
# Note, there are three versions where each version is formatted differently, 
# here we use version 2.
# URL to the XLSX file
xlsx_url <- "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/dfbcc8c9-a754-4cae-83c8-2f508d110ab2/resource/0fc80a2b-8d74-4e7c-8fa3-22e497d05a97/download/City%20of%20Toronto%20Climate%20Perceptions%202021%20v1%20.xlsx"

# Download the file to a temporary file using httr::GET
response <- GET(xlsx_url, write_disk(tf <- tempfile(fileext = ".xlsx")))

# Check if the response is successful (HTTP 200)
if (response$status_code != 200) {
  stop("Failed to download file: HTTP ", response$status_code)
}

#### Read all sheets ####
# List all sheet names in the Excel file
sheet_names <- excel_sheets(tf)

# Create a list to store all sheets' data
all_sheets <- lapply(sheet_names, function(sheet) {
  read_excel(tf, sheet = sheet)
})

#### Save to a single Excel file with all 122 sheets ####
# Initialize a new workbook
wb <- createWorkbook()

# Add each sheet to the workbook
for (i in seq_along(sheet_names)) {
  addWorksheet(wb, sheet_names[i])
  writeData(wb, sheet_names[i], all_sheets[[i]])
}

# Save the workbook
saveWorkbook(wb, "data/01-raw_data/twenty_twenty_one_raw_data.xlsx", overwrite = TRUE)