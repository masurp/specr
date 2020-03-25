#' Plot how analytical choices affect results
#'
#' This functions plots how analytical choices affect the obtained results (i.e., the rank within the curve). Significant results are highlighted (negative = red, positive = blue, grey = nonsignificant). This functions creates the lower panel in \code{plot_specs()}.
#'
#' @param df a data frame resulting from \code{run_specs()}.
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
                         choices = c("x", "y", "model", "controls", "subsets"),
                         desc = FALSE,
                         null = 0) {

  value <- key <- NULL

  df %>%
    format_results(desc = desc, null = null) %>%
    dplyr::mutate(controls = ifelse(grepl("[+]", .data$controls), "all covariates", .data$controls)) %>%
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
