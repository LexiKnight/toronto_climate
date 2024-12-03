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

#### Read data ####
# Load your analysis data
climate_data <- read_csv(here::here("data/02-analysis_data/twenty_eighteen_individual_analysis_data.csv"))

#  confusion matrix allows us to understand our predictions and how well the model works. 
# for the summary table in results
# assess using the confusion matrix and chat to end up with 
# look at accuracy 

# Required fixes
# NA values in data
# how to save - what goes where? scripts, paper model section vs. summary table for results 



# Model for home improvement likelihood hil
# creating an 80 20 train test split cause cant assess our model on the same data that it is trained on. 
hil_train <- sample_n (climate_data, size = round(0.8*nrow(climate_data)))
hil_test <- anti_join(climate_data, hil_train)

# generating a decision tree to predict the home improvement likelihood based on age and education level
hilTree <- rpart(likelihood_home_improvement ~ age + highest_level_educ, hil_train)
plot(as.party(hilTree), gp = gpar(cex=1), type="simple")

# creating a confusion matrix based on our testing data
predictions <- predict(hilTree, newdata=hil_test, type="class")
confusion_matrix1 <- table(predictions, hil_test$likelihood_home_improvement)
print(confusion_matrix1)

# save as parquet
write_parquet(climate_data_cleaned, here::here("data/analysis_data/TODO.parquet"))



# Model for reduce hydro likelihood rhl
# 80 20 train test split 
rhl_train <- sample_n (climate_data, size = round(0.8*nrow(climate_data)))
rhl_test <- anti_join(climate_data, rhl_train)

# generating decision tree to predict the reduction of hydro likelihood based on age and education level
rhlTree <- rpart(likelihood_reduce_hydro ~ age + highest_level_educ, rhl_train)
plot(as.party(rhlTree), gp = gpar(cex=1), type="simple")

# creating a confusion matrix based on our testing data
predictions <- predict(rhlTree, newdata=rhl_test, type="class")
confusion_matrix2 <- table(predictions, rhl_test$likelihood_reduce_hydro)
print(confusion_matrix2)


# Model for minimize car likelihood mcl
# TODO: NOT WORKING (small plot)
# 80 20 train test split 
mcl_train <- sample_n (climate_data, size = round(0.8*nrow(climate_data)))
mcl_test <- anti_join(climate_data, mcl_train)

# generating decision tree to predict the minimizing car likelihood based on age and education level
mclTree <- rpart(likelihood_minimize_car ~ age + highest_level_educ, mcl_train)
plot(as.party(mclTree), gp = gpar(cex=1), type="simple")

# creating a confusion matrix based on our testing data
predictions <- predict(mclTree, newdata=mcl_test, type="class")
confusion_matrix3 <- table(predictions, mcl_test$likelihood_minimize_car)
print(confusion_matrix3)



# Model for electric vehicle likelihood evl
# 80 20 train test split 
evl_train <- sample_n (climate_data, size = round(0.8*nrow(climate_data)))
evl_test <- anti_join(climate_data, evl_train)

# generating decision tree to predict the electric vehicle likelihood based on age and education level
evlTree <- rpart(likelihood_vehicle_electric ~ age + highest_level_educ, evl_train)
plot(as.party(evlTree), gp = gpar(cex=1), type="simple")

# creating a confusion matrix based on our testing data
predictions <- predict(evlTree, newdata=evl_test, type="class")
confusion_matrix4 <- table(predictions, evl_test$likelihood_vehicle_electric)
print(confusion_matrix4)



# Model for protein alternative likelihood pal
# 80 20 train test split 
pal_train <- sample_n (climate_data, size = round(0.8*nrow(climate_data)))
pal_test <- anti_join(climate_data, pal_train)

# generating decision tree to predict the protein alternative likelihood based on age and education level
palTree <- rpart(likelihood_protein_alternative ~ age + highest_level_educ, pal_train)
plot(as.party(palTree), gp = gpar(cex=1), type="simple")

# creating a confusion matrix based on our testing data
predictions <- predict(palTree, newdata=pal_test, type="class")
confusion_matrix5 <- table(predictions, pal_test$likelihood_protein_alternative)
print(confusion_matrix5)



# Model for waste reduciton likelihood wrl
# 80 20 train test split 
wrl_train <- sample_n (climate_data, size = round(0.8*nrow(climate_data)))
wrl_test <- anti_join(climate_data, wrl_train)

# generating decision tree to predict the waste reduction likelihood based on age and education level
wrlTree <- rpart(likelihood_reduce_waste ~ age + highest_level_educ, wrl_train)
plot(as.party(wrlTree), gp = gpar(cex=1), type="simple")

# creating a confusion matrix based on our testing data
predictions <- predict(wrlTree, newdata=wrl_test, type="class")
confusion_matrix6 <- table(predictions, wrl_test$likelihood_reduce_waste)
print(confusion_matrix6)



# Model for green product likelihood gpl
# TO DO: DOESNT WORK
# 80 20 train test split 
gpl_train <- sample_n (climate_data, size = round(0.8*nrow(climate_data)))
gpl_test <- anti_join(climate_data, gpl_train)

# generating decision tree to predict the green product likelihood based on age and education level
gplTree <- rpart(likelihood_green_product ~ age + highest_level_educ, gpl_train)
plot(as.party(gplTree), gp = gpar(cex=1), type="simple")

# creating a confusion matrix based on our testing data
predictions <- predict(gplTree, newdata=gpl_test, type="class")
confusion_matrix7 <- table(predictions, gpl_test$likelihood_green_produt)
print(confusion_matrix7)



# Model for short distance likelihood sdl
# 80 20 train test split 
sdl_train <- sample_n (climate_data, size = round(0.8*nrow(climate_data)))
sdl_test <- anti_join(climate_data, sdl_train)

# generating decision tree to predict the short distance likelihood based on age and education level
sdlTree <- rpart(likelihood_short_distance ~ age + highest_level_educ, sdl_train)
plot(as.party(sdlTree), gp = gpar(cex=1), type="simple")

# creating a confusion matrix based on our testing data
predictions <- predict(sdlTree, newdata=sdl_test, type="class")
confusion_matrix8 <- table(predictions, sdl_test$likelihood_short_distance)
print(confusion_matrix8)



# Model for sort waste likelihood swl
# 80 20 train test split 
swl_train <- sample_n (climate_data, size = round(0.8*nrow(climate_data)))
swl_test <- anti_join(climate_data, swl_train)

# generating decision tree to predict the sort waste likelihood based on age and education level
swlTree <- rpart(likelihood_sort_waste ~ age + highest_level_educ, swl_train)
plot(as.party(swlTree), gp = gpar(cex=1), type="simple")

# creating a confusion matrix based on our testing data
predictions <- predict(swlTree, newdata=swl_test, type="class")
confusion_matrix9 <- table(predictions, swl_test$likelihood_sort_waste)
print(confusion_matrix9)

