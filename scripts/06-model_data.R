#### Preamble ####
# Purpose: Constructs a  decision tree model for predicting likelihood of taking climate action based on age and education. 
# Author: Lexi Knight
# Date: 30 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete 01-download_data.R and 03-clean_data.R in scripts folder in order to access data.



#### Workspace setup ####

# load packages
library(tidyverse)    # core functions for data manipulation
library(rpart)        # creates decision tree models
library(partykit)     # plots decision tree models
library(here)         # constructs file paths

# Set seed for reproducibility
set.seed(853)

#### Read data ####
climate_data <- read_csv(here::here("data/02-analysis_data/twenty_eighteen_individual_analysis_data.csv"))

#### Shorten education level names ####
# Function to map long education names to shorter, more concise names
shorten_education_levels <- function(education) {
  case_when(
    education == "High school or less" ~ "High School",
    education == "Some community college, vocational, trade school" ~ "Some Community / Trade School",
    education == "Completed community college, vocational, trade school" ~ "Community / Trade School",
    education == "Some university" ~ "Some University",
    education == "Completed undergraduate degree" ~ "Undergrad",
    education == "Post graduate/professional school" ~ "Postgrad",
    education == "Prefer not to answer" ~ "Pref no Answer",
    TRUE ~ education # Default: keep original value if not matched
  )
}

# Apply the function to shorten names in the dataset
climate_data <- climate_data %>%
  mutate(highest_level_educ = shorten_education_levels(highest_level_educ))

# Function to generate decision trees, predictions, confusion matrix, and accuracy
generate_model <- function(target_var, train_data, test_data, model_name) {
  # Generate decision tree
  formula <- as.formula(paste(target_var, "~ age + highest_level_educ"))
  tree <- rpart(formula, data = train_data)
  
  # Construct file path for saving the plot
  png_file_path <- file.path(here::here("models/"), paste0(model_name, "_tree.png"))
  
  # Adjust PNG size to avoid cutting off the tree
  png(png_file_path, width = 4800, height = 2400, res = 300)  # Larger size and higher resolution
  
  # Plot the decision tree and save the plot as a .png
  plot(as.party(tree), gp = gpar(cex = 1.5), type = "simple")  # Adjust font size for better visibility
  
  # Close the device after saving the plot
  dev.off()
  
  # Make predictions on the test dataset
  predictions <- predict(tree, newdata = test_data, type = "class")
  
  # Generate the confusion matrix
  confusion_matrix <- table(predictions, test_data[[target_var]])
  
  # Calculate and return accuracy
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix) * 100
  return(accuracy)
}


#### Loop to create models for different target variables ####
# Create a data frame to store the model names and their corresponding accuracies
model_results <- data.frame(Model = character(0), Accuracy = numeric(0))

# List of target variables and corresponding model names
target_vars <- c("likelihood_home_improvement", "likelihood_reduce_hydro", "likelihood_minimize_car", 
                 "likelihood_vehicle_electric", "likelihood_protein_alternative", "likelihood_reduce_waste", 
                 "likelihood_green_product", "likelihood_short_distance", "likelihood_sort_waste")

# Loop through each model, split the data, generate the model, and store the accuracy
for (target_var in target_vars) {
  # Train-test split
  train_data <- sample_n(climate_data, size = round(0.8 * nrow(climate_data)))
  test_data <- anti_join(climate_data, train_data)
  
  # Model name (from target variable), removing the "likelihood_" part
  model_name <- gsub("likelihood_", "", target_var)
  
  # Generate model and calculate accuracy
  accuracy <- generate_model(target_var, train_data, test_data, model_name)
  
  # Store the results in the data frame
  model_results <- rbind(model_results, data.frame(Model = model_name, Accuracy = accuracy))
}

#### Modify model_results ####
# Rename variables in the Model column
rename_models <- function(model_name) {
  case_when(
    model_name == "home_improvement" ~ "Home Improvement",
    model_name == "reduce_hydro" ~ "Reduce Hydro",
    model_name == "minimize_car" ~ "Minimize Car ",
    model_name == "vehicle_electric" ~ " Electric / Hybrid Vehicle",
    model_name == "protein_alternative" ~ "Meat Alternative",
    model_name == "reduce_waste" ~ "Reduce Waste",
    model_name == "green_product" ~ "Green Products",
    model_name == "short_distance" ~ "Walk / Cycle Short Distance",
    model_name == "sort_waste" ~ "Sort Waste Correctly",
    TRUE ~ model_name # Default: keep original value if not matched
  )
}

# Apply the renaming function
model_results <- model_results %>%
  mutate(Model = rename_models(Model))

# Print the results table
print(model_results)

# Save the results as a CSV in the correct location
write_csv(model_results, here::here("models/model_accuracies.csv"))


