#### Calculate Summarized Scores ----
calculate_scores_summarized <- function(test, real, test_rank, real_rank, theta1, theta2, variable) {
  data.frame(
    mae = (sum(abs(test - real))) / length(test),
    mae_when_not_same_sign = mean(c(abs(test - real))[test * real > 0]),
    rmse = sqrt((sum((test - real)^2)) / length(test)),
    pct_same_sign = mean((test * real) > 0),
    pct_same_rank = mean(test_rank == real_rank),
    avg_diff_in_rank_when_not_equal = mean(abs(test_rank - real_rank)[test_rank != real_rank]),
    direction_contrib = mean(ifelse((test * real) > 0, 1, min(1, (2 * theta1) / (abs(test) + abs(real) + theta1)))),
    relative_mag_contrib = mean(ifelse((1 + theta2) / (abs(test - real) + 1) > 1, 1, (1 + theta2) / (abs(test - real) + 1))),
    rank_contrib = mean(1 / (abs(test_rank - real_rank) + 1)),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      score = direction_contrib + relative_mag_contrib + rank_contrib,
      variable = variable
    )
}

#### Calculate Scores ----
calculate_scores <- function(test, real, test_rank, real_rank, theta1, theta2, variable) {
  data.frame(
    direction_contrib = ifelse((test * real) > 0, 1, min(1, (2 * theta1) / (abs(test) + abs(real) + theta1))),
    relative_mag_contrib = ifelse((1 + theta2) / (abs(test - real) + 1) > 1, 1, (1 + theta2) / (abs(test - real) + 1)),
    rank_contrib = 1 / (abs(test_rank - real_rank) + 1),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      score = direction_contrib + relative_mag_contrib + rank_contrib,
      variable = variable
    )
}

#### Compare mSHAP Values to kernelSHAP ----
compare_shap_vals <- function(test_shap, real_shap, theta1, theta2) {
  # Get rank of SHAP values for each variable
  rank_test <- test_shap %>% 
    select(-expected_value, -predicted_val) %>%
    t() %>% 
    as.data.frame() %>%
    mutate(across(everything(), ~rank(-abs(.x), ties.method = "average"))) %>%
    t() %>%
    as.data.frame() %>%
    magrittr::set_rownames(NULL) %>%
    magrittr::set_colnames(paste0("s", 1:ncol(.), "_rank"))
  rank_real <- real_shap %>% 
    select(-expected_value, -predicted_val) %>% 
    t() %>% 
    as.data.frame() %>%
    mutate(across(everything(), ~rank(-abs(.x), ties.method = "average"))) %>%
    t() %>%
    as.data.frame() %>%
    magrittr::set_rownames(NULL) %>%
    magrittr::set_colnames(paste0("s", 1:ncol(.), "_rank"))
  x <- purrr::pmap_dfr(
    .l = list(
      test = test_shap %>% select(-expected_value, -predicted_val),
      real = real_shap %>% select(-expected_value, -predicted_val),
      test_rank = rank_test,
      real_rank = rank_real,
      variable = colnames(test_shap %>% select(-expected_value, -predicted_val))
    ),
    .f = calculate_scores,
    theta1 = theta1,
    theta2 = theta2
  )
  y <- purrr::pmap_dfr(
    .l = list(
      test = test_shap %>% select(-expected_value, -predicted_val),
      real = real_shap %>% select(-expected_value, -predicted_val),
      test_rank = rank_test,
      real_rank = rank_real,
      variable = colnames(test_shap %>% select(-expected_value, -predicted_val))
    ),
    .f = calculate_scores_summarized,
    theta1 = theta1,
    theta2 = theta2
  )
  list(
    summarized = y,
    detailed = x
  )
}

#### Create Model and get SHAP values ----
py_shap_vals_kernel <- function(
  y1 = "rowSums(X)", 
  y2 = "x1 + x2",
  n_var = 10L,
  sample = 100L, 
  seed = 15L
) {
  np <- reticulate::import("numpy")
  pd <- reticulate::import("pandas")
  shap <- reticulate::import("shap")
  sklearn <- reticulate::import("sklearn.ensemble")
  gbr <- sklearn$GradientBoostingRegressor
  
  
  np$random$seed(seed)
  x1 <- np$random$uniform(low = -10L, high = 10L, size = sample)
  x2 <- np$random$uniform(low = 0L, high = 20L, size = sample)
  x3 <- np$random$uniform(low = -5L, high = -1L, size = sample)
  
  dat <- dict(x1 = x1, x2 = x2, x3 = x3)
  X <- pd$DataFrame(data = dat)
  
  y1 <- eval(parse(text = y1))
  y2 <- eval(parse(text = y2))
  y3 <- y1 * y2
  
  mod1 <- gbr(loss = "ls", min_samples_leaf = 2L)
  mod1$fit(X, y3)
  
  exy1 <- shap$TreeExplainer(mod1)
  y1ex <- exy1$expected_value
  shapy1 <- exy1$shap_values(X) %>% 
    as.data.frame() %>%
    magrittr::set_colnames(paste0("s", 1:ncol(.))) %>%
    mutate(expected_value = y1ex) %>%
    mutate(predicted_val = rowSums(.))
  
  pred <- function(X) {
    mod1$predict(X)
  }
  
  kernelex <- shap$KernelExplainer(py_func(pred), X)
  real_shap_vals <- kernelex$shap_values(X) %>%
    as.data.frame() %>%
    magrittr::set_colnames(paste0("s", 1:ncol(.), "_real")) %>%
    mutate(expected_value = kernelex$expected_value) %>%
    mutate(predicted_val = rowSums(.))
  
  list(
    shap1 = shapy1,
    ex1 = y1ex,
    real_shap = real_shap_vals
  )
}

#### Put it all together ----
test_kernel_shap <- function(
  y1 = "rowSums(X)", 
  y2 = "x1 + x2",
  sample = 100L, 
  theta1 = 0.8, 
  theta2 = 7,
  n_var = 20
) {
  usethis::ui_info("Comupting performance of multiplcative SHAP for {y1} and {y2} with theta1 = {theta1} and theta2 = {theta2} on {sample} samples")
  l <- py_shap_vals_kernel(y1, y2, sample = sample, n_var = n_var)
  
  res_unif <- compare_shap_vals(l$shap1, l$real_shap, theta1, theta2)
  
  res_unif$summarized %>%
    dplyr::mutate(n_var = n_var, y1 = y1, y2 = y2, theta1 = theta1, theta2 = theta2, sample = sample) %>%
    dplyr::select(n_var, y1, y2, theta1, theta2, sample, variable, dplyr::everything())
}