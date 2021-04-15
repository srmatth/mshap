#' SHAP Summary Plot
#' 
#' A Function for obtaining a beeswarm plot, similar to the summary plot
#' in the `{shap}` python package.
#' 
#' This function allows the user to pass a data frame of SHAP values and
#' variable values and returns a ggplot object displaying a general summary
#' of the effect of Variable level on SHAP value by variable.
#' It is created with `{ggbeeswarm}`, and the returned value is a `{ggplot2}`
#' object that can be modified for given themes/colors.
#' 
#' Please note that for the `variable_values` and `shap_values` arguments,
#' both of which are data frames, the columns must be in the same order.
#' This is essential in assuring that the variable values and labels are 
#' matched to the correct shap values.
#'
#' @param variable_values A data frame of the  values of the variables that 
#'   caused the given SHAP values, generally will be the same data frame or 
#'   matrix that was passed to the model for prediction.
#' @param shap_values A data frame of shap values, either returned by `mshap()` 
#'   or obtained from the python `{shap}` module.
#' @param names A character vector of variable names, corresponding to the
#'   order of the columns in both `variable_values` and `shap_values`. If
#'   `NULL` (default), then the column names of the `variable_values` are 
#'   taken as `names`.
#'
#' @return A `{ggplot2}` object
#' @export
#'
#' @examples
#' # run vignette("mshap", package = "mshap") for examples
summary_plot <- function(variable_values, shap_values, names = NULL) {
  ## Check for packages
  if (!("ggplot2" %in% rownames(installed.packages()))) {
    stop("You must install the `{ggplot2}` package before running this function")
  }
  if (!("ggbeeswarm" %in% rownames(installed.packages()))) {
    stop("You must install the `{ggbeeswarm}` package before running this function")
  }
  if (!("tidyr" %in% rownames(installed.packages()))) {
    stop("You must install the `{tidyr}` package before running this function")
  }
  
  if (is.null(names)) {
    names <- colnames(variable_values)
  }
  
  variable_values <- variable_values %>%
    dplyr::mutate(
      dplyr::across(dplyr::everything(), .fns = ~((.x - min(.x)) / (max(.x) - min(.x))))
    ) %>%
    tidyr::pivot_longer(
      names_to = "variable",
      values_to = "var_val",
      cols = colnames(.)
    )
  
  important_vars <- shap_values %>%
    magrittr::set_colnames(names) %>%
    tidyr::pivot_longer(
      names_to = "variable",
      values_to = "value",
      cols = colnames(.)
    ) %>%
    dplyr::mutate(value = abs(value)) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarize(avg_value = mean(value)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(avg_value) %>%
    dplyr::pull(variable)
  
  shap_values %>%
    magrittr::set_colnames(names) %>%
    tidyr::pivot_longer(
      names_to = "variable",
      values_to = "value",
      cols = colnames(.)
    ) %>%
    dplyr::bind_cols(var_val = variable_values$var_val) %>%
    dplyr::mutate(variable = factor(variable, levels = important_vars)) %>%
    ggplot2::ggplot() +
    ggbeeswarm::geom_quasirandom(
      ggplot2::aes(y = variable, x = value, color = var_val),
      groupOnX = FALSE,
      size = 2
    ) + 
    ggplot2::theme_classic() +
    ggplot2::xlab("SHAP Value") +
    ggplot2::ylab("Variable") +
    ggplot2::scale_color_gradient2(
      high = "#0D3B66",
      mid = "#FAF0CA",
      low = "#A54657",
      midpoint = 0.5,
      breaks = c(0.1, 0.9),
      labels = c("Low", "High")
    ) +
    ggplot2::ggtitle("SHAP Value Summary") +
    ggplot2::labs(color = "Value of Variable") +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Times New Roman"),
      legend.position = c(0.8, 0.2),
      aspect.ratio = 0.75,
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18),
      legend.background = ggplot2::element_rect(color = "#0D3B66", fill = "#ffffff")
    ) +
    ggplot2::guides(
      color = ggplot2::guide_colorbar(
        label = TRUE,
        title.position = "top",
        title.hjust = 0.5,
        ticks = FALSE,
        direction = "horizontal",
        barwidth = ggplot2::unit(1.5, "in"),
        barheight = ggplot2::unit(0.1, "in")
      )
    )
}