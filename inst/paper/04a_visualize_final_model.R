## Script to create visualizations of final model

## Set the path to the data
dat_dir <- "inst/paper/pd_final/"

## load the mshap library
devtools::load_all() # for development
# library(mshap) # for when mshap is installed

## Load libraries
library(dplyr)
library(stringr)
library(readr)
library(fs)
library(purrr)
library(magrittr)
library(tidyr)
library(ggplot2)

## Read in the data we want
test <- read_csv(str_c(dat_dir, "pd_final_mod_test_data.csv")) %>%
  select(-X1) # this is the row number that came from saving the data in python
freq_preds <- read_csv(str_c(dat_dir, "pd_freq_preds_final_mod.csv")) %>%
  select(-X1)
sev_preds <- read_csv(str_c(dat_dir, "pd_sev_preds_final_mod.csv")) %>%
  select(-X1)

## Create some basic plots regarding the predictions
freq_preds %>%
  pivot_longer(
    cols = everything(),
    names_to = "num_claims",
    values_to = "prediction"
  ) %>%
  mutate(
    num_claims = case_when(
      num_claims == "p0" ~ "Probability of 0 Claims",
      num_claims == "p1" ~ "Probability of 1 Claim",
      num_claims == "p2" ~ "Probability of 2 Claims",
      num_claims == "p3" ~ "Probability of 3 Claims"
    )
  ) %>%
  ggplot() +
  aes(x = prediction, fill = num_claims) +
  geom_density(color = NA) +
  facet_wrap(~num_claims) +
  theme_classic() +
  ylab("Probability Density") +
  xlab("Predicted Probability") +
  ggtitle("Distribution of Frequency Model Predictions") +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("#A54657", "#849698", "#0D3B66", "#EE964B"))
  
sev_preds %>%
  set_colnames("prediction") %>%
  mutate(prediction = exp(prediction)) %>%
  ggplot() +
  aes(x = prediction) +
  geom_density(
    color = NA,
    fill = "#0D3B66"
  ) +
  theme_classic() +
  ylab("Probability Density") +
  xlab("Severity Prediction") +
  ggtitle("Distribution of Severity Predictions") +
  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5)
  )

## Read in the SHAP values from the model on the test set
freq_shap <- map(
  .x = dir_ls(dat_dir) %>% str_subset("freq_shap_values"),
  .f = ~{
    read_csv(.x) %>%
      select(-X1) %>%
      set_colnames(colnames(test))
  }
)
sev_shap <- read_csv(str_c(dat_dir, "pd_sev_shap_values_final_mod.csv")) %>%
  select(-X1) %>%
  set_colnames(colnames(test))

## Get the expected values of the individual SHAP calculations
## (from log file created in 04)
freq_ex <- c(0.18744214, 0.24445534, 0.28483949, 0.28326303)
sev_ex <- 7.53027045

## Compute the mSHAP values
final_mshap <- mshap(sev_shap, freq_shap, sev_ex, freq_ex)

## Calculate the explanations for the expected value
ev_explained <- final_mshap[[1]]$shap_vals * 0 + 
  final_mshap[[2]]$shap_vals * 1 + 
  final_mshap[[3]]$shap_vals * 2 +
  final_mshap[[4]]$shap_vals * 3

shap_expected_values <- final_mshap[[1]]$expected_value * 0 + 
  final_mshap[[2]]$expected_value * 1 + 
  final_mshap[[3]]$expected_value * 2 +
  final_mshap[[4]]$expected_value * 3

## Create the mshap plots
summary_plot(variable_values = test, shap_values = ev_explained)
observation_plot(
  variable_values = test[1,],
  shap_values = ev_explained[1,],
  expected_value = shap_expected_values,
  num_vars = 10
)



