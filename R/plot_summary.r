#' A boxplot summarizing parameters per analytical choice
#'
#' This function provides a convenient way to investigate the effect of individual choices on the estimate of interest. Similar to \code{summarise_specs}, it can be used to investigate individual choices (but visually). Further customization using \link[ggplot2]{ggplot} syntax is possible.
#'
#' @param df a data frame containing the choices and results of each specification (resulting from \code{run_specs}).
#' @param choices a vector specifying which analytical choices should be plotted. By default, all choices are plotted.
#'
#' @return a \link[ggplot2]{ggplot} object.
#'
#' @export
#'
#' @examples
#' # run spec analysis
#' results <- run_specs(example_data,
#'                      y = c("y1", "y2"),
#'                      x = c("x1", "x2"),
#'                      model = "lm",
#'                      controls = c("c1", "c2"),
#'                      subset = list(group1 = unique(example_data$group1)))
#'
#' # plot boxplot comparing specific choices
#' plot_summary(results, choices = c("subsets", "controls", "y"))
plot_summary <- function(df,
                         choices = c("x", "y", "model", "controls", "subsets")) {
  value <- key <- NULL

  df %>%
    dplyr::mutate(controls = ifelse(grepl("[+]", .data$controls), "all covariates", .data$controls)) %>%
    tidyr::gather(key, value, choices) %>%
    ggplot(aes(x = .data$value, y = .data$estimate, fill = .data$key)) +
      geom_boxplot(outlier.color = "red") +
      coord_flip() +
      scale_fill_brewer(palette = "Blues") +
      facet_grid(.data$key~1, scales = "free_y", space = "free") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.line = element_line("black", size = .5),
            axis.text = element_text(colour = "black"),
            strip.text.x = element_blank())
}


