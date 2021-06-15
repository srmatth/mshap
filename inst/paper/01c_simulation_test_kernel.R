source("inst/paper/00c_functions_test_kernel.R")

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
    "rowSums(X) * rowMeans(X^2)",
    "rowMeans(X^2) * rowSums(X^2)",
    "rowSums(X^2) * rowSums(2*X - X^2)",
    "rowSums(2*X - X^2) * rowMeans(exp(X))",
    "rowMeans(exp(X)) * rowSums((exp(X) / (X^2))) * 0.25",
    "0.25 * rowSums((exp(X) / (X^2))) * rowSums(X)"
  ),
  theta1 = c(3),
  theta2 = c(10),
  n_var = c(3, 5, 10, 20, 30, 40, 50)
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
readr::write_csv(all_tests, "inst/paper/all_tests_results_kernel_comp.csv")


all_tests <- readr::read_csv("inst/paper/all_tests_results_kernel_comp.csv")

all_tests %>% 
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

##### Nor fo mSHAP ----
base_eqn <- data.frame(
  y1 = c(
    "rowSums(X)",
    "rowMeans(X^2)",
    "rowSums(X^2)",
    "rowSums(2*X - X^2)",
    "rowMeans(exp(X))",
    "0.25 * rowSums((exp(X) / (X^2)))"
  ),
  y2 = c(
    "rowMeans(X^2)",
    "rowSums(X^2)",
    "rowSums(2*X - X^2)",
    "rowMeans(exp(X))",
    "0.25 * rowSums((exp(X) / (X^2)))",
    "rowSums(X)"
  ),
  theta1 = 3,
  theta2 = 10,
  n_var = 3
)

new_eqns <- base_eqn %>%
  rbind(base_eqn %>% mutate(n_var = 5)) %>%
  rbind(base_eqn %>% mutate(n_var = 10)) %>%
  rbind(base_eqn %>% mutate(n_var = 20)) %>%
  rbind(base_eqn %>% mutate(n_var = 30)) %>%
  rbind(base_eqn %>% mutate(n_var = 40)) %>%
  rbind(base_eqn %>% mutate(n_var = 50))

# simulate mSHAP and kernelSHAP on all rows of the created data frame
tic("big one")
all_tests_mshap <- purrr::pmap_dfr(
  .l = new_eqns,
  .f = test_mshap,
  sample = 100L
)
toc() # 4989.2 seconds (~1.4 hours)

# Write data to file, include code to read it back in again
readr::write_csv(all_tests_mshap, "inst/paper/all_tests_results_kernel_comp_mshap.csv")


all_tests_mshap <- readr::read_csv("inst/paper/all_tests_results_kernel_comp_mshap.csv")

all_tests_mshap %>% 
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
#### Combine the results ----

all_tests %>%
  mutate(type = "TreeSHAP") %>%
  bind_rows(
    all_tests_mshap %>%
      mutate(type = "mSHAP")
  ) %>%
  ggplot() +
  aes(x = n_var, y = score, color = type, fill = type) +
  geom_smooth(se = TRUE) +
  geom_hline(aes(yintercept = 3), color = "black", lty = 2) +
  scale_color_manual(values = c("#A54657", "#0D3B66")) +
  scale_fill_manual(values = c("#A54657", "#0D3B66")) +
  xlab("Number of Variables") +
  ylab("Score (out of 3)") +
  ggtitle(
    "Comparison of mSHAP and TreeSHAP", 
    "Score when Computed Against kernelSHAP with Similar Response Transformations"
  ) +
  guides(color = guide_legend(nrow = 1)) +
  theme_classic() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.title = element_blank(),
    axis.text = element_text(family = "Times New Roman"),
    axis.title = element_text(family = "Times New Roman"),
    legend.text = element_text(family = "Times New Roman"),
    legend.background = element_rect(color = "black"),
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
    plot.subtitle = element_text(hjust = 0.5, family = "Times New Roman")
  )


