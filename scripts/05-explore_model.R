#### Preamble ####
# Purpose: Explore and understand the spending model generated in 04-model.R by 
# creating data visualisations. Note visualisations of the species model are 
# contained in 99-replications.R.  
# Author: Julia Kim 
# Date: 24 March 2024 
# Contact: juliaym.kim@mail.utoronto.ca 
# License: MIT
# Pre-requisites: Run the 04-model.R script to produce the models. 

#### Workspace setup ####
library(tidyverse)
library(readr)
library(ggplot2)
library(patchwork)

#### Read data and model ####
cleaned_data <- na.omit(cleaned_species_data)

# Convert taxon and status to factors
cleaned_data$taxon <- factor(cleaned_data$taxon)
cleaned_data$status <- factor(cleaned_data$status)

# referenced code: 
# https://stackoverflow.com/questions/11291845/plot-the-results-of-a-multivariate-logistic-regression-model-in-r

# Create a data frame 
plotting_data <- data.frame(ngram_science = seq(min(cleaned_data$ngram_science), 
                                                max(cleaned_data$ngram_science),
                                                length.out = 100), 
                            ngram_common = seq(min(cleaned_data$ngram_common),
                                               max(cleaned_data$ngram_common),
                                                   length.out = 100),
                            taxon = levels(cleaned_data$taxon)[1],
                            status = levels(cleaned_data$status)[1], 
                            ngenus = seq(min(cleaned_data$ngenus), 
                                         max(cleaned_data$ngenus), 
                                         length.out = 100), 
                            listed = (0:1)) 

# Predict probabilities using the model
plotting_data$preds <- predict(species_listing_model_stanarm, newdata = plotting_data, 
                              type = "response")

# Plot probabilities against covariates 
ngram_science_plot <- ggplot(cleaned_species_data, aes(x = ngram_science, y = listed)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.2) + 
  geom_line(data = plotting_data, aes(x = ngram_science, y = preds), linewidth = 1) + 
  labs(
    x = expression(paste("Mean standardised science ", italic("n"), "-gram frequency")), 
    y = "Predicted Probability of Listing"
  )

ngram_common_plot <- ggplot(cleaned_species_data, aes(x = ngram_common, y = listed)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.2) + 
  geom_line(data = plotting_data, aes(x = ngram_common, y = preds), linewidth = 1) + 
  labs(
    x = expression(paste("Mean standardised common ", italic("n"), "-gram frequency")), 
    y = "Predicted Probability of Listing"
  )

log_ngenus_plot <- ggplot(cleaned_species_data, aes(x = log(ngenus), y = listed)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.2) + 
  geom_line(data = plotting_data, aes(x = log(ngenus), y = preds), linewidth = 1) + 
  labs(
    x = "Genus Size (logged)", 
    y = "Predicted Probability of Listing"
  )

# Combine plots using the patchwork package
combined_plots <- ngram_science_plot + ngram_common_plot + log_ngenus_plot
combined_plots <- combined_plots + plot_layout(ncol = 3)  # Arrange plots in 3 columns

# Print the combined plot
print(combined_plots)