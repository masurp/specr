#' Plot specification curve and analytical choices
#'
#' This function plots an entire visualization of the specification curve
#' analysis. The function uses the entire [tibble][tibble::tibble-package] that is produced by \code{run_specs()} to create a standard visualization of the specification curve analysis. Alternatively, one can also pass two separately created \link[ggplot2]{ggplot} objects to the function. In this case, it simply combines them using \code{cowplot::plot_grid}. Significant results are highlighted (negative = red, positive = blue, grey = nonsignificant).
#'
#' @param df a data frame resulting from \code{run_specs()}.
#' @param plot_a a ggplot object resulting from \code{plot_curve()} (or \code{plot_choices()} respectively).
#' @param plot_b a ggplot object resulting from \code{plot_choices()} (or \code{plot_curve()} respectively).
#' @param choices a vector specifying which analytical choices should be plotted. By default, all choices are plotted.
#' @param labels labels for the two parts of the plot
#' @param rel_heights vector indicating the relative heights of the plot.
#' @param desc logical value indicating whether the curve should the arranged in
#'   a descending order. Defaults to FALSE.
#' @param ci logical value indicating whether confidence intervals should be
#'   plotted.
#' @param ribbon logical value indicating whether a ribbon instead should be
#'   plotted.
#' @param null Indicate what value represents the 'null' hypothesis (defaults to
#'   zero).
#' @param sample_perc numeric value denoting what percentage of the
#'   specifications should be plotted. Needs to be strictly greater than 0 and smalle than 1.
#'   Defaults to 1 (= all specifications). Drawing a sample from all
#'   specification usually makes only sense of the number of specifications is
#'   very large and one wants to simplify the visualization.
#' @param ... additional arguments that can be passed to \code{plot_grid()}.
#'
#' @return a \link[ggplot2]{ggplot} object.
#'
#' @export
#'
#' @examples
#' # load additional library
#' library(ggplot2) # for further customization of the plots
#'
#' # run spec analysis
#' results <- run_specs(example_data,
#'                      y = c("y1", "y2"),
#'                      x = c("x1", "x2"),
#'                      model = "lm",
#'                      controls = c("c1", "c2"),
#'                      subset = list(group1 = unique(example_data$group1)))
#'
#' # plot results directly
#' plot_specs(results)
#'
#' # Customize each part and then combine
#' p1 <- plot_curve(results) +
#'   geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
#'   ylim(-3, 12) +
#'   labs(x = "", y = "regression coefficient")
#'
#' p2 <- plot_choices(results) +
#'   labs(x = "specifications (ranked)")
#'
#' plot_specs(plot_a = p1,    # arguments must be called directly!
#'            plot_b = p2,
#'            rel_height = c(2, 2))
#'@seealso \itemize{
#'  \item [plot_curve()] to plot only the specification curve.
#'  \item [plot_choices()] to plot only the choices panel.
#'  \item [plot_samplesizes()] to plot a histogram of sample sizes per specification.
#'}
plot_specs <- function(df = NULL,
                       plot_a = NULL,
                       plot_b = NULL,
                       choices = c("x", "y", "model", "controls", "subsets"),
                       labels = c("A", "B"),
                       rel_heights = c(2, 3),
                       desc = FALSE,
                       null = 0,
                       ci = TRUE,
                       ribbon = FALSE,
                       sample_perc = 1,
                       ...) {

  if (!is.null(df)) {

    if (sample_perc > 1 | sample_perc < 0) {
      stop("`sample_n` must be greater than 0 and less than 1!")
    }

  # Draw sample
  df <- dplyr::sample_n(df, size = sample_perc*nrow(df))

  # Create both plots
  plot_a <- plot_curve(df, ci = ci, ribbon = ribbon, desc = desc, null = null)
  plot_b <- plot_choices(df, choices = choices, desc = desc, null = null)

  }

  # Combine plots
  cowplot::plot_grid(plot_a,
                     plot_b,
                     labels = labels,
                     align = "v",
                     axis = "rbl",
                     rel_heights = rel_heights,
                     ncol = 1,
                     ...)

}



