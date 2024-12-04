#### Preamble ####
# Purpose: Constructs a linear regression model for predicting likelihood of taking climate action based on age and education. 
# Author: Lexi Knight
# Date: 30 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete 01-download_data.R and 03-clean_data.R in scripts folder in order to access data.


#### Workspace setup ####

# load packages
library(tidyverse)
library(readr)
library(here)
library(arrow)
library(rpart)
library(partykit)

# set seed for reproducibility
set.seed(123)

#### Read data ####
# Load your analysis data
climate_data <- read_csv(here::here("data/02-analysis_data/twenty_eighteen_individual_analysis_data.csv"))

# List to store model names and accuracies
model_accuracies <- data.frame(Model = character(), Accuracy = numeric(), stringsAsFactors = FALSE)

# Helper function to calculate accuracy
calculate_accuracy <- function(predictions, actual_values) {
  confusion_matrix <- table(predictions, actual_values)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix) * 100
  return(accuracy)
}

# Function to train and evaluate models
train_and_evaluate_model <- function(target_var, model_name) {
  # 80-20 Train-Test Split
  train_data <- sample_n(climate_data, size = round(0.8 * nrow(climate_data)))
  test_data <- anti_join(climate_data, train_data)
  
  # Generate a decision tree model
  model <- rpart(as.formula(paste(target_var, "~ age + highest_level_educ")), data = train_data)
  
  # Make predictions on the test dataset
  predictions <- predict(model, newdata = test_data, type = "class")
  
  # Calculate accuracy
  accuracy <- calculate_accuracy(predictions, test_data[[target_var]])
  
  # Save model's accuracy in the list
  model_accuracies <<- rbind(model_accuracies, data.frame(Model = model_name, Accuracy = accuracy))
  
  # Optionally, print the accuracy
  cat(paste("Accuracy of", model_name, "Model:", round(accuracy, 2), "%\n"))
}

# Evaluate each model

# Home Improvement Likelihood Model
train_and_evaluate_model("likelihood_home_improvement", "Home Improvement Likelihood")

# Reduce Hydro Likelihood Model
train_and_evaluate_model("likelihood_reduce_hydro", "Reduce Hydro Likelihood")

# Minimize Car Likelihood Model
train_and_evaluate_model("likelihood_minimize_car", "Minimize Car Likelihood")

# Electric Vehicle Likelihood Model
train_and_evaluate_model("likelihood_vehicle_electric", "Electric Vehicle Likelihood")

# Protein Alternative Likelihood Model
train_and_evaluate_model("likelihood_protein_alternative", "Protein Alternative Likelihood")

# Waste Reduction Likelihood Model
train_and_evaluate_model("likelihood_reduce_waste", "Waste Reduction Likelihood")

# Green Product Likelihood Model
train_and_evaluate_model("likelihood_green_product", "Green Product Likelihood")

# Short Distance Likelihood Model
train_and_evaluate_model("likelihood_short_distance", "Short Distance Likelihood")

# Sort Waste Likelihood Model
train_and_evaluate_model("likelihood_sort_waste", "Sort Waste Likelihood")

# Print the summary table of model accuracies
print(model_accuracies)

# Save the accuracy table as a CSV or a report
write.csv(model_accuracies, here::here("data/02-analysis_data/model_accuracies.csv"), row.names = FALSE)
