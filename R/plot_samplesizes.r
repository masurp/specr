#' Plot sample sizes
#'
#' This function plots a histogram of sample sizes per specification. It can be added to the overall specification curve plot (see vignettes).
#'
#' @param df a data frame resulting from \code{run_specs()}.
#' @param desc logical value indicating whether the curve should the arranged in a descending order. Defaults to FALSE.
#'
#' @return a \link[ggplot2]{ggplot} object.
#'
#' @export
#'
#' @examples
#' # load additional library
#' library(ggplot2) # for further customization of the plots
#'
#' # run specification curve analysis
#' results <- run_specs(df = example_data,
#'                      y = c("y1", "y2"),
#'                      x = c("x1", "x2"),
#'                      model = c("lm"),
#'                      controls = c("c1", "c2"),
#'                      subsets = list(group1 = unique(example_data$group1),
#'                                     group2 = unique(example_data$group2)))
#' # plot ranked bar chart of sample sizes
#' plot_samplesizes(results)
#'
#' # customize
#' plot_samplesizes(results) +
#'   geom_hline(yintercept = median(results$obs),
#'              color = "darkgrey",
#'              linetype = "dashed") +
#'   theme_linedraw()
plot_samplesizes <- function(df,
                             desc = FALSE) {

  df %>%
    format_results(desc = desc) %>%
    ggplot(aes(x = .data$specifications,
               y = .data$obs)) +
    geom_bar(stat = "identity",
             fill = "grey",
             size = .2) +
    theme_minimal() +
    theme(
      axis.line = element_line("black", size = .5),
      legend.position = "none",
      panel.spacing = unit(.75, "lines"),
      axis.text = element_text(colour = "black")) +
    labs(x = "", y = "")
}
