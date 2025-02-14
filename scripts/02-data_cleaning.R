#### Preamble ####
# Purpose: Clean the raw dataset and saves it as a parquet file. 
# Author: Julia Kim 
# Date: 23 March 2024 
# Contact: juliaym.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: run 01-download_data.R, and download the relevant data as outlined in the README. 

#### Workspace setup ####
library(tidyverse)
library(arrow)

#### SPECIES DATASET CLEANING ####
raw_species_data <- read_csv("data/raw_data/speciesdata.csv")

cleaned_species_data <- 
  raw_species_data |> 
  mutate(
    # remove long tails of common and scientific ngrams by replacing values by NA 
    ngram_common = ifelse(ngram_common > 10, NA, ngram_common),
    ngram_science = ifelse(ngram_science > 10, NA, ngram_science),
    # remove ngrams with suspicious ratio
    ngram_common = ifelse(abs(ngram_common/ngram_science) > 10, NA, ngram_common), 
    # set listing to 0 for species whose NatureServe ranking was determined after delisting or before listing
    listed = ifelse(probs == 1, 0, listed)
  ) |> 
  # exclude rows where ngram_common_flag is equal to 1
  filter(ngram_common_flag != 1) |> 
  # remove "Extinct" and "Prob. Extinct" from status column
  filter(status != "Extinct" & status != "Prob. Extinct")  |> 
  # remove unneeded columns 
  select(-code, -family, -order, -name, -status_global, -evdist, -ge, -edge, -ngram_common_flag, -probs)  
  
#### Save data as parquet file ####
write_csv(cleaned_species_data, "data/analysis_data/cleaned_speciesdata.csv")
#write_parquet(cleaned_species_data, "data/analysis_data/cleaned_speciesdata.parquet")