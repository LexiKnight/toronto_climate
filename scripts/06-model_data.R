#### Preamble ####
# Purpose: Constructs a  decision tree model for predicting likelihood of taking climate action based on age and education. 
# Author: Lexi Knight
# Date: 30 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete 01-download_data.R and 03-clean_data.R in scripts folder in order to access data.


#### Workspace setup ####

# load packages
library(tidyverse)
library(rpart)
library(partykit)
library(here)
library(arrow)

# Set seed for reproducibility
set.seed(853)


#### Read data ####
climate_data <- read_csv(here::here("data/02-analysis_data/twenty_eighteen_individual_analysis_data.csv"))

# Function to generate decision trees, predictions, confusion matrix, and accuracy
generate_model <- function(target_var, train_data, test_data, model_name) {
  # Generate decision tree
  formula <- as.formula(paste(target_var, "~ age + highest_level_educ"))
  tree <- rpart(formula, data = train_data)
  
  # Construct file path for saving the plot
  png_file_path <- file.path(here::here("data/04-model_data"), paste0(model_name, "_tree.png"))
  
  # Plot the decision tree and save the plot as a .png
  png(png_file_path)
  plot(as.party(tree), gp = gpar(cex = 1), type = "simple")
  dev.off()
  
  # Make predictions on the test dataset
  predictions <- predict(tree, newdata = test_data, type = "class")
  
  # Generate the confusion matrix
  confusion_matrix <- table(predictions, test_data[[target_var]])
  
  # Calculate and return accuracy
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix) * 100
  return(accuracy)
}

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

# Print the results table
print(model_results)

# Save the results as a CSV in the correct location
write_csv(model_results, here::here("data/04-model_data/model_accuracies.csv"))
