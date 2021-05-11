library(ggplot2)
library(dplyr)

all_tests <- readr::read_csv("mSHAP/all_tests_results_arbitrary_vars.csv")

all_tests%>% 
  # group_by(n_var, method) %>%
  # summarize(
  #   mean_score = mean(score)
  # ) %>%
  # ungroup() %>% 
  mutate(
    method = case_when(
      method == "uniform" ~ "Uniformly Distributed",
      method == "weighted_abs" ~ "Weighted by Absolute Values",
      method == "weighted_raw" ~ "Weighted by Raw Values",
      method == "weighted_squared" ~ "Weighted by Squared Values"
    )
  ) %>%
  ggplot() +
  aes(x = n_var, y = score, color = method) +
  # geom_point() +
  # geom_line() +
  scale_color_viridis_d() +
  geom_smooth(se = FALSE, method = "loess") +
  theme_classic() + 
  xlab("Number of Variables") +
  ylab("Avg Score") +
  labs(color = "Method") +
  theme(
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(
      color = "#000000",
      size = 0.5
    )
  )


#### Kernel SHAP comparison to TreeSHAP

all_tests <- readr::read_csv("mSHAP/all_tests_results_kernel_comp.csv")

all_tests %>% 
  ggplot() +
  aes(x = n_var, y = score) +
  geom_smooth(se = FALSE, method = "loess", color = "#440154") +
  # geom_point() +
  theme_classic() + 
  xlab("Number of Variables") +
  ylab("Avg Score") +
  labs(color = "Method") +
  theme(
    legend.position = c(0.8, 0.8)
  )
