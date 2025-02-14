#### Preamble ####
# Purpose: Download the raw data 
# Author: Julia Kim
# Date: 2 April 2024 
# Contact:juliaym.kim@mail.utoronto.ca
# License: MIT
# Pre-requisites: To run this code, speciesdata.csv must be downloaded in advance. 
# This can be done either by downloading the file 
# from data/raw_data in this paper's repository or from Moore et al.'s replication package
# at https://dataverse.harvard.edu/file.xhtml?fileId=4931734&version=1.0 

#### Workspace setup #### 
library(tidyverse) 
library(readr)

#### Download datasets #### 
species_data <- read.csv(here::here("data/raw_data/speciesdata.csv"))