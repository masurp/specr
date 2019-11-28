#' Plot specification curve and analytical choices
#'
#' This function plots an entire visualization of the specification curve analysis. It can be used in two ways:
#' 1. The results frame returned by \code{run_specs()} is passed directly. In this case, the function plots the entire visualization automatically.
#' 2. The ggplot objects returned from \code{plot_curve()} and \code{plot_choices()} are passed to the function. In this case, the function simply arranges them above each other.
#'
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
#' # run spec analysis
#' results <- run_specs(example_data,
#'                      y = c("y1", "y2"),
#'                      x = c("x1", "x2"),
#'                      model = "lm",
#'                      controls = c("c1", "c2"),
#'                      subset = list(gender = unique(d$gender)))
#'
#' # plot results directly
#' plot_specs(results)
#'
#' # customize each part and then combine
#' p1 <- plot_curve(results) +
#'   geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
#'   ylim(-3, 12) +
#'   labs(x = "", y = "unstandarized regression coefficient")
#'
#' p2 <- plot_choices(results) +
#'   labs(y = "analytical choices")
#'
#' p3 <- plot_variance(results)
#'
#' plot_specs(plot_a = p1,
#'            plot_b = p2,
#'            rel_height = c(2, 2))
#'
#'
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
              axis = "rbl",
              rel_heights = rel_heights,
              ncol = 1)

}



