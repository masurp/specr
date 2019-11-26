#' Plot specification curve and analytical choices
#'
#' @param df data frame resulting from \code{run_specs()}.
#' @param plot_a a ggplot object resulting from \code{plot_curve()}.
#' @param plot_b a ggplot object resulting from \code{plot_choices()}.
#' @param labels labels for the two parts of the plot
#' @param rel_heights vector indicating the relative heights of the plot.
#' @param desc logical value indicating whether the curve should the arranged in a descending order. Defaults to FALSE.
#' @param ci logical value indicating whether confidence intervals should be plotted.
#' @param prob numeric value indicating what type of confidence intervals should be plotted. Defaults to .975 (= 95% confidence intervalls.)
#'
#' @return
#' @export
#'
#' @examples
plot_specs <- function(df = NULL,
                       plot_a = NULL,
                       plot_b = NULL,
                       labels = c("A", "B"),
                       rel_heights = c(2, 3),
                       desc = FALSE,
                       ci = TRUE,
                       prob = .975) {

  # dependencies
  require(cowplot)

  if (!is_null(df)) {
  plot_a <- plot_curve(df, ci = ci, prob = prob, desc = desc)
  plot_b <- plot_choices(df, desc = desc)

  }

  plot_grid(plot_a, plot_b,
            labels = labels,
            align = "v",
            axis = "b",
            rel_heights = rel_heights,
            ncol = 1)
}



