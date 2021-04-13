summary_plot <- function(variable_values, shap_values, names = NULL) {
  variable_values <- variable_values %>%
    dplyr::mutate(
      dplyr::across(dplyr::everything(), .fns = ~((.x - min(.x)) / (max(.x) - min(.x))))
    ) %>%
    tidyr::pivot_longer(
      names_to = "variable",
      values_to = "var_val",
      cols = colnames(.)
    )
  
  if (is.null(names)) {
    names <- colnames(variable_values)
  }
  
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