#### Preamble ####
# Purpose: Simulate dataset 
# Author: Julia Kim 
# Date: 22 March 2024 
# Contact: juliaym.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: none

#### Workspace setup ####
library(tidyverse)

#### Simulate data ####
set.seed(867) # for reproducibility 

n <- 50 # number of simulated observations 

simulated_species_data <- 
  tibble(
    # use 1 through 50 to represent each species 
    species_id = c(1:n),
    # randomly assign taxon for each species_id 
    taxon = sample(c("Birds", "Mammals", "Amphibians", "Reptiles",
                     "Protists", "Invertebrates", "Plants", "Fish", "Fungi"), 
                   size = n, replace = TRUE),
    # randomly assign indicator variable to denote listed or not listed 
    listed = sample(0:1, size = n, replace = TRUE), 
    # randomly assign NatureServe assessment status 
    status = sample(c(1, 2, 3, 4, 5, "UNK"), size = n, replace = TRUE),
    # randomly assign scientific name ngram frequency 
    ngram_science = rnorm(n = 50, mean = -0.1, sd = 1), 
    # randomly assign common name ngram frequency
    ngram_common = rnorm(n = 50, mean = -0.02, sd = 0.05), 
    # randomly assign number of genus in species
    ngenus = sample(1:10, size = n, replace = TRUE)
  )

# TESTS # 
# Check there are 50 species in the dataset 
nrow(simulated_species_data) == 50

# Check listed is a binary variable that takes on 0 or 1 
all(simulated_species_data$listed %in% c(0, 1)) == TRUE 

# Check that there are exactly 9 unique taxa to which a species can belong  
length(unique(simulated_species_data$taxon)) == 9

# Check that there are nine taxa in the dataset 
all(simulated_species_data$taxon %in% c("Birds", "Mammals", "Amphibians", "Reptiles",
                     "Protists", "Invertebrates", "Plants", "Fish", "Fungi")) == TRUE 

# Check that status is a value between 1 to 5 inclusive or "UNK" 
all(simulated_species_data$status %in% c(1, 2, 3, 4, 5, "UNK")) == TRUE 

# Check that the genus number is larger than 0 
all(simulated_species_data$ngenus > 0) == TRUE 

# Check each column type 
simulated_species_data$taxon |> class() == "character"
simulated_species_data$listed |> class() == "integer"
simulated_species_data$status |> class() == "character"
simulated_species_data$ngram_science |> class() == "numeric"
simulated_species_data$ngram_common |> class() == "numeric"
simulated_species_data$ngenus |> class() == "integer"