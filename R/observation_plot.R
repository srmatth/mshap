observation_plot <- function(variable_values, shap_values, expected_value, names = NULL) {
  if (is.null(names)) {
    names <- colnames(variable_values)
  }
  predicted_value <- rowSums(shap_values) + expected_value
  individual_shap <- shap_values %>%
    magrittr::set_colnames(names) %>%
    tidyr::pivot_longer(
      names_to = "covariate",
      values_to = "shap_val",
      cols = colnames(.)
    ) %>%
    dplyr::full_join(
      variable_values %>%
        tidyr::pivot_longer(
          cols = colnames(.),
          names_to = "covariate",
          values_to = "var_val"
        ),
      by = c("covariate")
    )
  d <- individual_shap %>%
    dplyr::mutate(covariate = forcats::fct_reorder(covariate, shap_val, abs)) %>%
    dplyr::arrange(desc(covariate)) %>%
    dplyr::mutate(
      bar_end = cumsum(shap_val) + c(expected_value),
      bar_start = c(expected_value, head(bar_end, -1)),
      is_positive = shap_val > 0
    ) 
  d %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = covariate, 
      xmin = as.numeric(covariate) - 0.25, 
      xmax = as.numeric(covariate) + 0.25, 
      ymin = bar_end, 
      ymax = bar_start, 
      label = var_val,
      fill = is_positive
    ) + 
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = expected_value),
      color = "#849698",
      size = 1.5
    ) +
    ggplot2::geom_rect(stat = "identity") +
    ggplot2::scale_fill_manual(values = setNames(c("#A54657", "#0D3B66"), c(FALSE, TRUE))) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::geom_segment(
      ggplot2::aes(
        y = bar_end,
        yend = bar_end,
        x = as.numeric(covariate) + 0.25,
        xend = as.numeric(covariate) - 1.25
      ),
      color = "#849698",
      lwd = 1
    ) +
    ggplot2::theme_classic() +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = predicted_value),
      color = "#EE964B",
      size = 1.5
    ) +
    ggplot2::scale_y_continuous(
      breaks = expected_value,
      labels = stringr::str_c(
        "Baseline:\n", 
        signif(expected_value, 6)
      ),
      sec.axis = ggplot2::sec_axis(
        ~.,
        breaks = predicted_value,
        labels = stringr::str_c(
          "Model Prediction:\n", 
          signif(predicted_value, 6)
        )
      )
    ) +
    ggplot2::scale_x_discrete(
      labels = rev(stringr::str_c(d$covariate, "\n", signif(d$var_val, 3)))
    ) +
    ggplot2::ggtitle(stringr::str_c("Individual Observation Explanation")) +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Times New Roman"),
      legend.position = "none",
      axis.text.y = ggplot2::element_text(
        family = "Times New Roman",
        size = 10
      ),
      axis.text.x = ggplot2::element_text(
        family = "Times New Roman",
        size = 12,
        hjust = 0.5
      ),
      axis.title = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    )
}