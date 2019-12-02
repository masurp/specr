#' Plot specification curve
#'
#' This function plots the a ranked specification curve. Confidence intervals can be included. Significant specifications are highlighted. Further customization with ggplot2 is possible.
#'
#' @param df a data frame containing the choices and results of each specification (resulting from \code{run_specs}).
#' @param desc logical value indicating whether the curve should the arranged in a descending order. Defaults to FALSE.
#' @param ci logical value indicating whether confidence intervals should be plotted.
#' @param prob numeric value indicating what type of confidence intervals should be plotted. Defaults to .975 (= 95% confidence intervalls.)
#' @param legend logical value indicating whether the legend should be plotted Defaults to FALSE.
#' @param or should odds ratio be computed?
#'
#' @return
#' @export
#'
#' @examples
plot_curve <- function(df,
                       desc = FALSE,
                       ci = TRUE,
                       prob = .975,
                       legend = FALSE,
                       or = FALSE){

  require(ggplot2)
  require(dplyr)

  # rank specs
  if (isFALSE(desc)) {
    df <- df %>%
      arrange(estimate)
  } else {
    df <- df %>%
      arrange(desc(estimate))
  }

  # Create basic plot
  plot <- df %>%
    format_results(prob = prob, or = or) %>%
    ggplot(aes(x = specifications,
               y = estimate,
               ymin = ll,
               ymax = ul,
               color = color)) +
    geom_point(aes(color = color),
               size = 1) +
    theme_minimal() +
    scale_color_identity() +
    theme(strip.text = element_blank(),
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          panel.spacing = unit(.75, "lines"),
          axis.text = element_text(colour = "black")) +
    labs(x = "")

  if (isFALSE(legend)) {
    plot <- plot +
      theme(legend.position = "none")
  }

  # add CIs if necessary
  if (isTRUE(ci)) {
    plot <- plot +
      geom_pointrange(alpha = 0.5,
                      size = .6,
                      fatten = 1)
  }

  return(plot)
}
