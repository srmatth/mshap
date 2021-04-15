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
mshap <- function(
  shap_1, 
  shap_2, 
  ex_1, 
  ex_2,
  shap_1_names = NULL,
  shap_2_names = NULL
) {
  
  if ("list" %in% class(shap_1) & "list" %in% class(shap_2)) {
    stop("`mshap::mshap()` is not currently set up to handle multiple matrices in each `shap_*` argument.  Did you accidentally wrap a matrix in a list?")
  } else if ("list" %in% class(shap_1) | "list" %in% class(shap_2)) {
    if ("list" %in% class(shap_1)) {
      main <- shap_1
      main_ex <- ex_1
      secondary <- shap_2
      sec_ex <- ex_2
    } else {
      main <- shap_2
      main_ex <- ex_2
      secondary <- shap_1
      sec_ex <- ex_1
    }
    
    l <- purrr::map2(
      .x = main,
      .y = ex_1,
      .f = ~{
        multiply_shap(
          shap_1 = .x,
          shap_2 = secondary,
          ex_1 = .y,
          ex_2 = sec_ex,
          shap_1_names = shap_1_names,
          shap_2_names = shap_2_names
        )
      }
    )
    
  } else {
    l <- multiply_shap(
      shap_1,
      shap_2,
      ex_1,
      ex_2,
      shap_1_names,
      shap_2_names
    )
  }
  return(l)
}