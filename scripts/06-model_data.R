#### Preamble ####
# Purpose: Constructs a linear regression model for predicting likelihood of taking climate action based on age and education. 
# Author: Lexi Knight
# Date: 30 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites: complete 01-download_data.R and 03-clean_data.R in scripts folder in order to access data.


#### Workspace setup ####

library(tidyverse)
library(readr)
library(here)
library(caret)
library(arrow)
library(rpart)
library(partykit)

#### Read data ####
# Load your analysis data
climate_data <- read_csv(here::here("data/02-analysis_data/twenty_eighteen_individual_analysis_data.csv"))

# Check the levels of highest_level_educ
table(climate_data$highest_level_educ)

# creating an 80 20 train test split cause cant assess our model on the same data that it is trained on. 
hil_train <- sample_n (climate_data, size = round(0.8*nrow(climate_data)))
hil_test <- anti_join(climate_data, hil_train)

# generating a decision tree to predict the home improvement likelihood based on age and education level
hilTree <- rpart(likelihood_home_improvement ~ age + highest_level_educ, hil_train)
plot(as.party(hilTREE), gp = gpar(cex=1), type="simple")

# creating a confusion matrix based on our testing data
predictions <- predict(hilTree, newdata=hil_test, type="class")
confusion_matrix <- table(predictions, hil_test$likelihood_home_improvement)
print(confusion_matrix)

#  confusion matrix allows us to understand our predictions and how well the model works. 


# for the summary table in results
# assess using the confusion matrix and chat to end up with 
# look at accuracy 



  # Save the model and cleaned data
  saveRDS(climate_model, here::here("models/climate_model_home_improvement.rds"))
  write_parquet(climate_data_cleaned, here::here("data/analysis_data/climate_action_home_improvement_model_data.parquet"))
  

