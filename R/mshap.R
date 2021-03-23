#' mSHAP
#' 
#' A function for calculation SHAP values of two part models
#' 
#' This function allows the user to input the SHAP values for two separate 
#' models (along with the expected values), and mSHAP then outputs the SHAP
#' values of the two model predictions multiplied together.
#'
#' @param shap_1 SHAP values for the data set from the first model
#' @param shap_2 SHAP values for the data set from the second model
#' @param ex_1 the expected value of the explainer for the first model
#' @param ex_2 the expected value of the explainer for the second model
#'
#' @return a list containing the multiplied SHAP values and the expected value
#' @export
mshap <- function(shap_1, shap_2, ex_1, ex_2) {
  ## Error Checking
  # check the dimensions of the SHAP values (they need to be the same)
  if (min(dim(shap_1) == dim(shap_2)) == FALSE) {
    stop("`shap1` and `shap2` must have the same dimensions")
  }
  # Check the column types of shap_1 and shap_2
  shap_1_class <- purrr::map_chr(shap_1, class)
  shap_2_class <- purrr::map_chr(shap_2, class)
  if (min(c(shap_1_class, shap_2_class) %in% c("integer", "numeric")) == FALSE) {
    stop("`shap1` and `shap2` must be only composed of numerical values")
  }
  # Check the type of the input on the expected values
  if (!is.numeric(ex_1) | !is.numeric(ex_2)) {
    stop("`ex_1` and `ex_2` must be numeric")
  }
  # Check the length of the expected value inputs
  if (length(ex_1) > 1) {
    warning("`ex1` has a length greater than 1, only using first element")
    ex_1 <- ex_1[1]
  }
  if (length(ex_2) > 1) {
    warning("`ex2` has a length greater than 1, only using first element")
    ex_2 <- ex_2[1]
  }
  
  d <- purrr::map_dfc(
    .x = 1:ncol(shap_1),
    .f = ~{
      (shap_1 %>% dplyr::pull(.x)) * c(ex_2) + 
        (shap_2 %>% dplyr::pull(.x)) * c(ex_1) + 
        ((shap_1 %>% dplyr::pull(.x)) * (shap_2 %>% rowSums())) / 2 +
        ((shap_1 %>% rowSums()) * (shap_2 %>% dplyr::pull(.x))) / 2
    }
  ) %>%
    magrittr::set_colnames(colnames(shap_1))
  
  expected_value <- d %>% 
    rowSums() %>% 
    mean()
  
  tot_s <- rowSums(abs(d))
  shap_vals <- purrr::map_dfc(
    .x = d,
    .f = ~{
      .x + (abs(.x) / tot_s) * (ex_1 * ex_2 - expected_value)
    }
  )
  
  # return a list with what we want
  list(
    shap_vals = shap_vals,
    expected_value = expected_value
  )
}