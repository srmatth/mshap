#### Simulation Study for Distributing Alpha ####

# This file contains the code and process for carrying out the simulation study
# It also contains additional code for testing the time difference between mSHAP and kernelSHAP

#### Setup ----

# Source the functions file
source("mSHAP/00_functions.R")

# Load Libraries
library(tictoc)
library(ggplot2)
library(dplyr)

#### First Simulation ----

# Create a data frame of values to map across
eqns <- list(
  y1 = c(
    "x1 + x2 + x3",
    "2*x1 + 2*x2 + 3*x3"
  ),
  y2 = c(
    "x1 + x2 + x3",
    "2*x1 + 2*x2 + 3*x3",
    "x1 * x2 * x3",
    "x1^2 * x2^3 * x3^4",
    "(x1 + x2) / (x1 + x2 + x3)",
    "x1 * x2 / (x1 + x1 * x2 + x1^2 * x3^2)"
  ),
  theta1 = seq(0.5, 20.5, by = 1),
  theta2 = seq(1, 50, by = 5)
) %>%
  expand.grid(stringsAsFactors = FALSE)

# simulate mSHAP and kernelSHAP on all rows of the created data frame
tic("big one")
all_tests <- purrr::pmap_dfr(
  .l = eqns,
  .f = test_multiplicative_shap,
  sample = 100L
)
toc() # 11021.87 seconds (~3 hours)

# Write data to file, include code to read it back in again
readr::write_csv(all_tests, "mSHAP/all_tests_results.csv")
all_tests <- readr::read_csv("mSHAP/all_tests_results.csv")

#### Distribute Alpha Winner ----

all_tests %>%
  group_by(method) %>%
  summarise(mean_score = mean(score)) %>%
  ungroup() %>%
  arrange(desc(mean_score)) # weighted_abs is the best for the score

all_tests %>%
  group_by(method) %>%
  summarise(mean_dir_con = mean(direction_contrib)) %>%
  ungroup() %>%
  arrange(desc(mean_dir_con)) # weighted abs is the best for the direction

all_tests %>%
  group_by(method) %>%
  summarise(mean_rel_val_con = mean(relative_mag_contrib)) %>%
  ungroup() %>%
  arrange(desc(mean_rel_val_con)) # weighted abs is the best for the relative magnitude

all_tests %>%
  group_by(method) %>%
  summarise(mean_rank_con = mean(rank_contrib)) %>%
  ungroup() %>%
  arrange(desc(mean_rank_con)) # weighted abs is the best for the rank contribution

all_tests %>%
  group_by(method) %>%
  summarise(mean_mae = mean(mae)) %>%
  ungroup() %>%
  arrange(mean_mae) # uniform is the best for the mean_mae

all_tests %>%
  group_by(method) %>%
  summarise(pct_same_sign = mean(pct_same_sign)) %>%
  ungroup() %>%
  arrange(desc(pct_same_sign)) # weighted_abs is the best with the pct same sign

all_tests %>%
  group_by(method) %>%
  summarise(pct_same_rank = mean(pct_same_rank)) %>%
  ungroup() %>%
  arrange(desc(pct_same_rank)) # weighted squared is the best with the pct same rank (just .0005 ahead of weighted abs)

# A summary of all metrics
summary <- all_tests %>%
  group_by(method) %>%
  summarise(
    mean_score = mean(score),
    mean_dir_con = mean(direction_contrib),
    mean_rel_val_con = mean(relative_mag_contrib),
    mean_rank_con = mean(rank_contrib),
    pct_same_sign = mean(pct_same_sign),
    pct_same_rank = mean(pct_same_rank)
  ) %>%
  ungroup() %>%
  arrange(desc(mean_score))

View(summary)

#### Timing Simulations ----

## First, with a fixed number of variables and increasing sample size

# Create the list of values to map over
params <- list(
  sample = c(100L, 200L, 500L, 1000L, 5000L, 10000L), 
  vars = c(2L)
) %>% expand.grid()

# Perform the simulation
all_times_2_vars <- purrr::map2_dfr(
  .x = params$sample,
  .y = params$vars,
  .f = compare_times
)

# Save and load the results
readr::write_csv(all_times_2_vars, "Multiplicative SHAP/all_times_2_vars.csv")
all_times_2_vars <- readr::read_csv("Multiplicative SHAP/all_times_2_vars.csv")

## Then, with a fixed sample size and increasing variables

# Create the list of values to map over
params <- list(
  sample = c(100L),
  vars = c(2:20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100)
) %>% expand.grid()

# Perform the Simulation
all_times_100_sample <- purrr::map2_dfr(
  .x = params$sample,
  .y = params$vars,
  .f = compare_times
)

# Save and load the results
readr::write_csv(all_times_100_sample, "Multiplicative SHAP/all_times_100_sample.csv")
all_times_100_sample <- readr::read_csv("Multiplicative SHAP/all_times_100_sample.csv")