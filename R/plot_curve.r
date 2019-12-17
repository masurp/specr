#' Plot specification curve
#'
#' This function plots the a ranked specification curve. Confidence intervals can be included. Significant specifications are highlighted. Further customization with ggplot2 is possible. The function \code{plot_specs()} uses this function to create the overall plot. One can, however, use this function to customize parts of the overall specification curve plot more specifically.
#'
#' @param df a data frame containing the choices and results of each specification (resulting from \code{run_specs}).
#' @param desc logical value indicating whether the curve should the arranged in a descending order. Defaults to FALSE.
#' @param ci logical value indicating whether confidence intervals should be plotted.
#' @param legend logical value indicating whether the legend should be plotted Defaults to FALSE.
#' @param null Indicate what value represents the null hypothesis (Defaults to zero)
#'
#' @return
#' @export
#'
#' @examples
#' # Run specification curve analysis
#' results <- run_specs(df = example_data,
#'                      y = c("y1", "y2"),
#'                      x = c("x1", "x2"),
#'                      model = c("lm"),
#'                      controls = c("c1", "c2"),
#'                      subsets = list(group1 = unique(example_data$group1),
#'                                     group2 = unique(example_data$group2)))
#' # Plot simple specification curve
#' plot_curve(results, ci = FALSE)
#'
#' # With confidence intervals and customize further
#' plot_curve(results) +
#'   geom_hline(yintercept = 0) +
#'   geom_hline(yintercept = median(results$estimate), linetype = "dashed") +
#'   theme_bw()
plot_curve <- function(df,
                       desc = FALSE,
                       ci = TRUE,
                       legend = FALSE,
                       null = 0){

  require(ggplot2, quietly = TRUE)
  require(dplyr, quietly = TRUE)

  # Create basic plot
  plot <- df %>%
    format_results(desc = desc, null = null) %>%
    ggplot(aes(x = specifications,
               y = estimate,
               ymin = conf.low,
               ymax = conf.high,
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
