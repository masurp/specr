#' Plot ranked specification curve
#'
#' @description `r lifecycle::badge("deprecated")`
#'    This function is deprecated because the new version of specr uses a new analytic framework.
#'    In this framework, you can plot a similar figure simply by using the generic \code{plot()} function and
#'    adding the argument \code{type = "curve"}.
#'    This function plots the a ranked specification curve. Confidence intervals can be included. Significant results are highlighted (negative = red, positive = blue, grey = nonsignificant). This functions creates the upper panel in \code{plot_specs()}.
#'
#' @param df a data frame resulting from \code{run_specs()}.
#' @param var which variable should be evaluated? Defaults to estimate (the effect sizes computed by [run_specs()]).
#' @param group Should the arrangement of the curve be grouped by a particular choice?
#'    Defaults to NULL, but can be any of the present choices (e.g., x, y, controls...)
#' @param desc logical value indicating whether the curve should the arranged in a descending order. Defaults to FALSE.
#' @param ci logical value indicating whether confidence intervals should be plotted.
#' @param ribbon logical value indicating whether a ribbon instead should be plotted.
#' @param legend logical value indicating whether the legend should be plotted Defaults to FALSE.
#' @param null Indicate what value represents the null hypothesis (Defaults to zero)
#'
#' @return a \link[ggplot2]{ggplot} object.
#' @export
#'
#' @examples
#' # load additional library
#' library(ggplot2) # for further customization of the plots
#'
#' # Run specification curve analysis
#' results <- run_specs(df = example_data,
#'                      y = c("y1", "y2"),
#'                      x = c("x1", "x2"),
#'                      model = c("lm"),
#'                      controls = c("c1", "c2"),
#'                      subsets = list(group1 = unique(example_data$group1),
#'                                     group2 = unique(example_data$group2)))
#'
#' # Plot simple specification curve
#' plot_curve(results)
#'
#' # Ribbon instead of CIs and customize further
#' plot_curve(results, ci = FALSE, ribbon = TRUE) +
#'   geom_hline(yintercept = 0) +
#'   geom_hline(yintercept = median(results$estimate),
#'              linetype = "dashed") +
#'   theme_linedraw()
plot_curve <- function(df,
                       var = .data$estimate,
                       group = NULL,
                       desc = FALSE,
                       ci = TRUE,
                       ribbon = FALSE,
                       legend = FALSE,
                       null = 0){

  # Deprecation warning
  lifecycle::deprecate_warn("1.0.0", "plot_curve()", "plot.specr.object()")

  var <- enquo(var)
  group <- enquo(group)

  # Create basic plot
  plot <- df %>%
    format_results(var = var, group = group, null = null, desc = desc) %>%
    ggplot(aes(x = .data$specifications,
               y = !! var,
               ymin = .data$conf.low,
               ymax = .data$conf.high,
               color = .data$color)) +
    geom_point(aes(color = .data$color),
               size = 1) +
    theme_minimal() +
    scale_color_identity() +
    theme(strip.text = element_blank(),
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          panel.spacing = unit(.75, "lines"),
          axis.text = element_text(colour = "black")) +
    labs(x = "")

  # add legends if necessary
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

  # add ribbon if necessary
  if (isTRUE(ribbon)) {
    plot <- plot +
      geom_ribbon(aes(ymin = .data$conf.low,
                      ymax = .data$conf.high,
                      color = "lightgrey"),
                      alpha = 0.25)
  }
  return(plot)
}
