#### Create Plots in the Paper ####

# This file will create the various plots used in the mSHAP paper

## Score Plots ----
all_tests <- readr::read_csv("mSHAP/all_tests_results.csv")

all_tests %>% 
  # filter(
  #   y1 == "x1 + x2 + x3",
  #   y2 == "x1 + x2 + x3"
  # ) %>%
  group_by(method, theta1, theta2) %>%
  summarize(score = mean(score)) %>%
  ungroup() %>%
  mutate(
    method = case_when(
      method == "uniform" ~ "Uniformly Distributed",
      method == "weighted_abs" ~ "Weighted by Absolute Values",
      method == "weighted_raw" ~ "Weighted by Raw Values",
      method == "weighted_squared" ~ "Weighted by Squared Values"
    )
  ) %>%
  ggplot() +
  aes(x = theta1, y = score, color = as.factor(theta2)) +
  facet_wrap(~method) + 
  theme_bw() + 
  xlab(expression(paste("Value of ", theta[1]))) +
  ylab("Average Score") +
  ggtitle(
    expression(paste("Effect of ", theta[1],  " on Score")), 
    expression(paste("Shown by Method and ", theta[2]))
  ) +
  labs(color = expression(theta[2])) +
  scale_color_viridis_d() +
  geom_smooth(se = FALSE, method = "lm") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

all_tests %>% 
  # filter(
  #   y1 == "x1 + x2 + x3",
  #   y2 == "x1 + x2 + x3"
  # ) %>%
  group_by(method, theta1, theta2) %>%
  summarize(score = mean(score)) %>%
  ungroup() %>%
  mutate(
    method = case_when(
      method == "uniform" ~ "Uniformly Distributed",
      method == "weighted_abs" ~ "Weighted by Absolute Values",
      method == "weighted_raw" ~ "Weighted by Raw Values",
      method == "weighted_squared" ~ "Weighted by Squared Values"
    )
  ) %>%
  ggplot() +
  aes(x = theta2, y = score) +
  # geom_point() +
  facet_wrap(~method) + 
  theme_bw() +
  xlab(expression(paste("Value of ", theta[2]))) +
  ylab("Average Score") +
  ggtitle(
    expression(paste("Effect of ", theta[2], " on Score")), 
    "Shown by Method"
  ) +
  # labs(color = "Theta 1") +
  geom_smooth(se = FALSE, color = "#440154")  +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

## Tiing Plots ----
all_times_2_vars <- readr::read_csv("inst/paper/all_times_2_vars.csv")

all_times_2_vars %>%
  tidyr::pivot_longer(
    cols = c("kernel_time", "multiplicative_time"),
    names_to = "method",
    values_to = "time"
  ) %>%
  mutate(method = ifelse(method == "kernel_time", "KernelSHAP", "mSHAP")) %>%
  ggplot() +
  aes(x = smaple_size, y = time, color = method) +
  geom_line(lwd = 1.5) +
  theme_classic() +
  scale_color_manual(values = c("#A54657", "#0D3B66")) +
  xlab("Sample Size") +
  ylab("Time (seconds)") +
  ggtitle("Comparison of Time by Method", "Number of Variables Fixed at 2") +
  labs(color = "Method") +
  guides(color = guide_legend(nrow = 1, title.hjust = 0.5, title.position = "top")) +
  theme(
    legend.position = c(0.7, 0.3),
    legend.background = element_rect(color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, family = "Times New Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 18, family = "Times New Roman"),
    axis.text = element_text(size = 14, family = "Times New Roman"),
    axis.title = element_text(size = 16, family = "Times New Roman"),
    legend.text = element_text(size = 14, family = "Times New Roman"),
    legend.title = element_text(size = 16, family = "Times New Roman")
  )

all_times_100_sample <- readr::read_csv("inst/paper/all_times_100_sample.csv")

all_times_100_sample %>%
  tidyr::pivot_longer(
    cols = c("kernel_time", "multiplicative_time"),
    names_to = "method",
    values_to = "time"
  ) %>%
  mutate(method = ifelse(method == "kernel_time", "KernelSHAP", "mSHAP")) %>%
  ggplot() +
  aes(x = num_variables, y = time, color = method) +
  geom_line(lwd = 1.5) +
  theme_classic() +
  ylab("Time (seconds)") +
  xlab("Number of Variables")+
  scale_color_manual(values = c("#A54657", "#0D3B66")) +
  ggtitle("Comparison of Time by Method", "Sample Size Fixed at 100") +
  labs(color = "Method") +
  guides(color = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5)) +
  theme(
    legend.position = c(0.7, 0.3),
    legend.background = element_rect(color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 20, family = "Times New Roman"),
    plot.subtitle = element_text(hjust = 0.5, size = 18, family = "Times New Roman"),
    axis.text = element_text(size = 14, family = "Times New Roman"),
    axis.title = element_text(size = 16, family = "Times New Roman"),
    legend.text = element_text(size = 14, family = "Times New Roman"),
    legend.title = element_text(size = 16, family = "Times New Roman")
  )

