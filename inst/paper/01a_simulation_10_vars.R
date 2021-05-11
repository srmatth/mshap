source("mSHAP/00a_functions_10_vars.R")

# Load Libraries
library(tictoc)
library(ggplot2)
library(dplyr)
library(reticulate)

#### First Simulation ----

# Create a data frame of values to map across
eqns <- list(
  y1 = c(
    "rowSums(X)",
    "x1 - x2 + x3 - x4 + x5 - x6 +x7 - x8 + x9 - x10"
  ),
  y2 = c(
    "x1 * x2 + x3*x4 + x5*x6 + (x7*x8)/(x9 - x10)",
    "(x1 + x2 + x3 + x4 + x5) / (x6 *x7 * x8 * x9 * x10)",
    "x5 + x10",
    "exp(x1 + x2 + x3 + x4 + x5) - (x6 +x7 + x8 + x9 + x10)"
  ),
  theta1 = seq(0.5, 20.5, by = 5),
  theta2 = seq(1, 50, by = 10)
) %>%
  expand.grid(stringsAsFactors = FALSE)

# simulate mSHAP and kernelSHAP on all rows of the created data frame
tic("big one")
all_tests <- purrr::pmap_dfr(
  .l = eqns,
  .f = test_multiplicative_shap,
  sample = 100L
)
toc() # 16542.71 seconds (~4.6 hours)

# Write data to file, include code to read it back in again
readr::write_csv(all_tests, "mSHAP/all_tests_results_10_vars.csv")
all_tests <- readr::read_csv("mSHAP/all_tests_results_10_vars.csv")

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
