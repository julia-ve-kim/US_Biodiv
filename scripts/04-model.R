#### Preamble ####
# Purpose: Make models of the data to generate inferential statistics.
# Author: Julia Kim 
# Date: 23 March 2024 
# Contact: juliaym.kim@mail.utoronto.ca 
# License: MIT
# Pre-requisites: Downloaded the data files through 01-download_data.R and through the instructions in the README

#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(here)

#### Read data ####
cleaned_species_data <- read_csv(here("data/analysis_data/cleaned_speciesdata.csv"))

#### Model data #### 
# set reference level of taxon in to "Mammals"
cleaned_species_data$taxon <- factor(cleaned_species_data$taxon)
cleaned_species_data$taxon <- relevel(cleaned_species_data$taxon, ref = "Mammals")

# fit a logistic regression model, with listed as the response variable 
species_listing_model <- glm(listed ~ taxon + status + ngram_common + ngram_science + I(log(ngenus)),  
                     data = cleaned_species_data, 
                     family = binomial(link = "logit"))

species_listing_model_stanarm <- stan_glm(listed ~ taxon + status + ngram_common + ngram_science + I(log(ngenus)),  
                             data = cleaned_species_data, 
                             family = binomial(link = "logit"),
                             prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
                             prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
                             seed = 853)

# run the logistic regression model without the observations with high Cook's distances 
cleaned_species_data_Cook <- cleaned_species_data[-c(1718, 4458, 4703), ]
species_listing_model_Cook <- stan_glm(listed ~ taxon + status + ngram_common + ngram_science + I(log(ngenus)),  
                             data = cleaned_species_data_Cook, 
                             family = binomial(link = "logit"),
                             prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
                             prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
                             seed = 853)

#### Save models ####
saveRDS(
  species_listing_model,
  file = "models/species_listing_model.rds"
)

saveRDS(
  species_listing_model_Cook,
  file = "models/species_listing_model_Cook.rds"
)

#### Summarise models #### 
summary(species_listing_model) 
summary(species_listing_model_Cook) 