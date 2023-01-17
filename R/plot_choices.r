#' Plot how analytical choices affect results
#'
#' @description `r lifecycle::badge("deprecated")`
#'    This function is deprecated because the new version of specr uses a new analytic framework.
#'    In this framework, you can plot a similar figure simply by using the generic \code{plot()} function.
#'    and adding the argument \code{type = "choices"}.
#'    This functions plots how analytic choices affect the obtained results (i.e., the rank within the curve). Significant results are highlighted (negative = red, positive = blue, grey = nonsignificant). This functions creates the lower panel in \code{plot_specs()}.
#'
#' @param df a data frame resulting from \code{run_specs()}.
#' @param var which variable should be evaluated? Defaults to estimate (the effect sizes computed by [run_specs()]).
#' @param group Should the arrangement of the curve be grouped by a particular choice?
#'    Defaults to NULL, but can be any of the present choices (e.g., x, y, controls...)
#' @param choices a vector specifying which analytical choices should be plotted. By default, all choices are plotted.
#' @param desc logical value indicating whether the curve should the arranged in a descending order. Defaults to FALSE.
#' @param null Indicate what value represents the 'null' hypothesis (Defaults to zero).
#'
#' @return a \link[ggplot2]{ggplot} object.
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
#'
#' # Plot simple table of choices
#' plot_choices(results)
#'
#' # Plot only specific choices
#' plot_choices(results,
#'              choices = c("x", "y", "controls"))
plot_choices <- function(df,
                         var = .data$estimate,
                         group = NULL,
                         choices = c("x", "y", "model", "controls", "subsets"),
                         desc = FALSE,
                         null = 0) {

  # Deprecation warning
  lifecycle::deprecate_warn("1.0.0", "plot_choices()", "plot.specr.object()")

  value <- key <- NULL

  var <- enquo(var)
  group <- enquo(group)

  # Create basic plot
  df %>%
    format_results(var = var, group = group, null = null, desc = desc) %>%
    tidyr::gather(key, value, choices) %>%
    dplyr::mutate(key = factor(.data$key, levels = choices)) %>%
    ggplot(aes(x = .data$specifications,
               y = .data$value,
               color = .data$color)) +
    geom_point(aes(x = .data$specifications,
                   y = .data$value),
               shape = 124,
               size = 3.35) +
    scale_color_identity() +
    theme_minimal() +
    facet_grid(.data$key~1, scales = "free_y", space = "free_y") +
    theme(
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          panel.spacing = unit(.75, "lines"),
          axis.text = element_text(colour = "black"),
          strip.text.x = element_blank()) +
    labs(x = "", y = "")

}
