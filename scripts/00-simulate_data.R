#### Preamble ####
# Purpose: Tests stimulation for age, education, extent informed, likelihood of taking climate actions and prefered method of communication data from the climate change perspectives dataset from OpenDataToronto.
# Author: Lexi Knight
# Date: 6 November 2024
# Contact: lexi.knight@mail.utoronto.ca
# License: MIT
# Pre-requisites:none

#### Workspace setup ####
# load packages
library(tidyverse)
library(arrow)

#### Simulate data ####

# Set seed for reproducibility
set.seed(853)

# Number of entries
num_entries <- 404

# Simulate climate change data
simulated_data <- data.frame(
  # Simulate age
  age = sample(18:100, num_entries, replace = TRUE),  # Simulating age from 18 to 100
  
  # education categories
  education = sample(c("high school or less",
                       "some community college / trade school",
                       "completed community college / trade school", 
                       "some university",
                       "completed undergraduate degree",
                       "post graduate / professional school",
                       "prefer not to answer"),
                     num_entries, replace = TRUE),
  
  # extent informed categories
  informed = sample(c("extremely informed",
                      "very informed",
                      "not very informed",
                      "not at all informed"),
                    num_entries, replace = TRUE),
  
  # likelihood of taking action categories 
  likelihood = sample(c("already doing this or have done this",
                        "very likely",
                        "somewhat likely",
                        "somewhat unlikely",
                        "very unlikely"),
                      num_entries, replace = TRUE),  # Corrected this line
  
  # communication method categories
  communication = sample(c("Toronto.ca website", 
                           "city of toronto events",
                           "twitter",
                           "facebook",
                           "instagram",
                           "city of toronto enewsletter / email",
                           "councillor communications",
                           "advertising campaigns",
                           "brochures / pamphlets",
                           "other",
                           "not interested"),
                         num_entries, replace = TRUE)
)

# Show a summary of the simulated data - as a check
summary(simulated_data)

# Save the simulated data
write_parquet(simulated_data, "data/00-simulated_data/simulated_climate_change_data.parquet")
