#### Functions for the simulation ####

# This file contains functions that help carry out the simulation on distributing alpha
# The functions are not documented, but only get used in the simulation
# The order/process of how the simulation was carried out can be found in the simulation.R file

#### Multiply SHAP ----
multiply_shap <- function(shap1, shap2, ex1, ex2) {
  # Error Checking
  if (min(dim(shap1) == dim(shap2)) == FALSE) {
    stop("`shap1` and `shap2` must have the same dimensions")
  }
  if (length(ex1) > 1) {
    warning("`ex1` has a length greater than 1, only using first element")
    ex1 <- ex1[1]
  }
  if (length(ex2) > 1) {
    warning("`ex2` has a length greater than 1, only using first element")
    ex2 <- ex2[1]
  }
  
  d <- purrr::map_dfc(
    .x = 1:ncol(shap1),
    .f = ~{
      (shap1 %>% dplyr::pull(.x)) * c(ex2) + 
        (shap2 %>% dplyr::pull(.x)) * c(ex1) + 
        ((shap1 %>% dplyr::pull(.x)) * (shap2 %>% rowSums())) / 2 +
        ((shap1 %>% rowSums()) * (shap2 %>% dplyr::pull(.x))) / 2
    }
  ) %>%
    magrittr::set_colnames(stringr::str_c("s", 1:ncol(shap1)))
  
  preds_1 <- rowSums(shap1 %>% dplyr::mutate(ex_val = ex1))
  preds_2 <- rowSums(shap2 %>% dplyr::mutate(ex_val = ex2))
  
  preds_3 <- preds_1 * preds_2
  
  expected_value <- mean(preds_3)
  
  # return a list with what we want
  list(
    vals = d,
    ex1ex2 = ex1 * ex2,
    ex3 = expected_value,
    alpha = ex1 * ex2 - expected_value
  )
}

#### Distribute Alpha ----
distribute_alpha <- function(multiplied_shap, method = "uniform") {
  if (method == "uniform") {
    d <- purrr::map_dfc(
      .x = multiplied_shap$vals,
      .f = ~{
        .x + (multiplied_shap$alpha / ncol(multiplied_shap$vals))
      }
    )
  } else if (method == "weighted_raw") {
    tot_s <- rowSums(multiplied_shap$vals)
    d <- purrr::map_dfc(
      .x = multiplied_shap$vals,
      .f = ~{
        .x + (.x / tot_s) * (multiplied_shap$alpha)
      }
    )
  } else if (method == "weighted_absolute") {
    tot_s <- rowSums(abs(multiplied_shap$vals))
    d <- purrr::map_dfc(
      .x = multiplied_shap$vals,
      .f = ~{
        .x + (abs(.x) / tot_s) * (multiplied_shap$alpha)
      }
    )
  } else if (method == "weighted_squared") {
    tot_s <- rowSums(multiplied_shap$vals^2)
    d <- purrr::map_dfc(
      .x = multiplied_shap$vals,
      .f = ~{
        .x + ((.x ^ 2) / tot_s) * (multiplied_shap$alpha)
      }
    )
  }
  
  d %>%
    dplyr::mutate(expected_value = multiplied_shap$ex3) %>%
    dplyr::mutate(predicted_val = rowSums(.))
}

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
py_shap_vals <- function(y1 = "x1 + x2 + x3", y2 = "2*x1 + 2*x2 + 2*x3", sample = 100L, seed = 15L) {
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
  mod1$fit(X, y1)
  
  mod2 <- gbr(loss = "ls", min_samples_leaf = 2L)
  mod2$fit(X, y2)
  
  exy1 <- shap$TreeExplainer(mod1)
  y1ex <- exy1$expected_value
  shapy1 <- exy1$shap_values(X) %>% as.data.frame()
  
  exy2 = shap$TreeExplainer(mod2)
  y2ex = exy2$expected_value
  shapy2 = exy2$shap_values(X) %>% as.data.frame()
  
  pred <- function(X) {
    mod1$predict(X) * mod2$predict(X)
  }
  
  kernelex <- shap$KernelExplainer(py_func(pred), X)
  real_shap_vals <- kernelex$shap_values(X) %>%
    as.data.frame() %>%
    magrittr::set_colnames(paste0("s", 1:ncol(.), "_real")) %>%
    mutate(expected_value = kernelex$expected_value) %>%
    mutate(predicted_val = rowSums(.))
  
  list(
    shap1 = shapy1,
    shap2 = shapy2,
    ex1 = y1ex,
    ex2 = y2ex,
    real_shap = real_shap_vals
  )
}

#### Put it all together ----
test_multiplicative_shap <- function(
  y1 = "x1 + x2 + x3", 
  y2 = "2*x1 + 2*x2 + 2*x3", 
  sample = 100L, 
  theta1 = 0.8, 
  theta2 = 7
) {
  usethis::ui_info("Comupting performance of multiplcative SHAP for {y1} and {y2} with theta1 = {theta1} and theta2 = {theta2} on {sample} samples")
  l <- py_shap_vals(y1, y2, sample = sample)
  
  test_shap <- multiply_shap(l$shap1, l$shap2, c(l$ex1), c(l$ex2))
  
  shap_unif <- test_shap %>% distribute_alpha("uniform")
  shap_w_raw <- test_shap %>% distribute_alpha("weighted_raw")
  shap_w_sq <- test_shap %>% distribute_alpha("weighted_squared")
  shap_w_abs <- test_shap %>% distribute_alpha("weighted_absolute")
  
  res_unif <- compare_shap_vals(shap_unif, l$real_shap, theta1, theta2)
  res_raw <- compare_shap_vals(shap_w_raw, l$real_shap, theta1, theta2)
  res_sq <- compare_shap_vals(shap_w_sq, l$real_shap, theta1, theta2)
  res_abs <- compare_shap_vals(shap_w_abs, l$real_shap, theta1, theta2)
  
  res_unif$summarized %>%
    mutate(method = "uniform") %>%
    rbind(res_raw$summarized %>% mutate(method = "weighted_raw")) %>%
    rbind(res_abs$summarized %>% mutate(method = "weighted_abs")) %>%
    rbind(res_sq$summarized %>% mutate(method = "weighted_squared")) %>%
    dplyr::mutate(y1 = y1, y2 = y2, theta1 = theta1, theta2 = theta2, sample = sample) %>%
    dplyr::select(y1, y2, theta1, theta2, sample, method, variable, dplyr::everything())
}

#### compare times between mSHAP and kernelSHAP ----
compare_times <- function(sample, vars) {
  np <- reticulate::import("numpy")
  pd <- reticulate::import("pandas")
  shap <- reticulate::import("shap")
  sklearn <- reticulate::import("sklearn.ensemble")
  gbr <- sklearn$GradientBoostingRegressor
  
  
  np$random$seed(16L)
  
  X <- purrr::map_dfc(
    .x = 1:as.integer(vars),
    .f = ~{
      np$random$uniform(low = -10L, high = 10L, size = as.integer(sample))
    }
  ) %>%
    magrittr::set_colnames(
      stringr::str_c("x", 1:vars)
    )
  
  y1 <- rowSums(X)
  y2 <- rowSums(2 * X)
  
  mod1 <- gbr(loss = "ls", min_samples_leaf = 2L)
  mod1$fit(X, y1)
  
  mod2 <- gbr(loss = "ls", min_samples_leaf = 2L)
  mod2$fit(X, y2)
  
  tictoc::tic("Multiplicative SHAP")
  exy1 <- shap$TreeExplainer(mod1)
  y1ex <- exy1$expected_value
  shapy1 <- exy1$shap_values(X) %>% as.data.frame()
  
  exy2 = shap$TreeExplainer(mod2)
  y2ex = exy2$expected_value
  shapy2 = exy2$shap_values(X) %>% as.data.frame()
  
  multiplied_shap <- multiply_shap(shapy1, shapy2, c(y1ex), c(y2ex)) %>%
    distribute_alpha("weighted_absolute")
  multiplied_time <- tictoc::toc()
  
  tictoc::tic("kernel ex")
  pred <- function(X) {
    mod1$predict(X) * mod2$predict(X)
  }
  
  kernelex <- shap$KernelExplainer(py_func(pred), X[1:100,])
  real_shap_vals <- kernelex$shap_values(X) %>%
    as.data.frame() %>%
    magrittr::set_colnames(paste0("s", 1:ncol(.), "_real")) %>%
    mutate(expected_value = kernelex$expected_value) %>%
    mutate(predicted_val = rowSums(.))
  kernel_time <- tictoc::toc()
  
  data.frame(
    smaple_size = sample,
    num_variables = vars,
    kernel_time = kernel_time$toc - kernel_time$tic,
    multiplicative_time = multiplied_time$toc - multiplied_time$tic
  )
}
