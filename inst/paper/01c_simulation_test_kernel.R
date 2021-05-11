source("mSHAP/00c_functions_test_kernel.R")

# Load Libraries
library(tictoc)
library(ggplot2)
library(dplyr)
library(reticulate)
library(stringr)

#### First Simulation ----

# Create a data frame of values to map across
eqns <- list(
  y1 = c(
    "rowSums(X)",
    "rowMeans(X^2)",
    "rowSums(X^2)",
    "rowSums(2*X - X^2)",
    "rowMeans(exp(X))",
    "0.25 * rowSums((exp(X) / (X^2)))"
  ),
  theta1 = c(3),
  theta2 = c(10),
  n_var = c(10, 20, 30, 40, 50)
) %>%
  expand.grid(stringsAsFactors = FALSE)

# simulate mSHAP and kernelSHAP on all rows of the created data frame
tic("big one")
all_tests <- purrr::pmap_dfr(
  .l = eqns,
  .f = test_kernel_shap,
  sample = 100L
)
toc() # 4989.2 seconds (~1.4 hours)

# Write data to file, include code to read it back in again
readr::write_csv(all_tests, "mSHAP/all_tests_results_kernel_comp.csv")
all_tests <- readr::read_csv("mSHAP/all_tests_results_kernel_comp.csv")

#### Distribute Alpha Winner ----

all_tests %>%
  summarise(mean_score = mean(score)) %>%
  ungroup() %>%
  arrange(desc(mean_score)) # weighted_abs is the best for the score

all_tests %>%
  summarise(mean_dir_con = mean(direction_contrib)) %>%
  ungroup() %>%
  arrange(desc(mean_dir_con)) # weighted abs is the best for the direction

all_tests %>%
  summarise(mean_rel_val_con = mean(relative_mag_contrib)) %>%
  ungroup() %>%
  arrange(desc(mean_rel_val_con)) # weighted abs is the best for the relative magnitude

all_tests %>%
  summarise(mean_rank_con = mean(rank_contrib)) %>%
  ungroup() %>%
  arrange(desc(mean_rank_con)) # weighted abs is the best for the rank contribution

all_tests %>%
  summarise(mean_mae = mean(mae)) %>%
  ungroup() %>%
  arrange(mean_mae) # uniform is the best for the mean_mae

all_tests %>%
  summarise(pct_same_sign = mean(pct_same_sign)) %>%
  ungroup() %>%
  arrange(desc(pct_same_sign)) # weighted_abs is the best with the pct same sign

all_tests %>%
  summarise(pct_same_rank = mean(pct_same_rank)) %>%
  ungroup() %>%
  arrange(desc(pct_same_rank)) # weighted squared is the best with the pct same rank (just .0005 ahead of weighted abs)

# A summary of all metrics
summary <- all_tests %>%
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

all_tests <- readr::read_csv("mSHAP/all_tests_results_kernel_comp.csv")

all_tests%>% 
  # group_by(n_var) %>%
  # summarize(
  #   mean_score = mean(score)
  # ) %>%
  # ungroup() %>%
  ggplot() +
  aes(x = n_var, y = score) +
  # geom_point() +
  # geom_line() +
  scale_color_viridis_d() +
  geom_smooth(se = FALSE) +
  theme_classic() + 
  xlab("Number of Variables") +
  ylab("Avg Score") +
  labs(color = "Method") +
  theme(
    legend.position = c(0.8, 0.8)
  )
