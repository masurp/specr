#' Plot decision tree
#'
#' This function plots a simple decision tree that is meant to help understanding how few analytical choices may results in a large number of specifications. It is somewhat useless if the final number of specifications is very high.
#'
#' @param df data frame resulting from [run_specs()].
#' @param label Logical. Should labels be included? Defaults to FALSE. Produces only a reasonable plot if number of specifications is low.
#' @param legend Logical. Should specific decisions be identifiable. Defaults to FALSE.
#'
#' @return a \link[ggplot2]{ggplot} object.
#'
#' @export
#'
#' @examples
#' results <- run_specs(df = example_data,
#'                     y = c("y1", "y2"),
#'                     x = c("x1", "x2"),
#'                     model = c("lm"),
#'                     controls = c("c1", "c2"))
#'
#' # Basic, non-labelled decisions tree
#' plot_decisiontree(results)
#'
#' # Labelled decisions tree
#' plot_decisiontree(results, label = TRUE)
#'
#' # Add legend
#' plot_decisiontree(results, label = TRUE, legend = TRUE)
plot_decisiontree <- function(df,
                              label = FALSE,
                              legend = FALSE) {

  # Create data set for graph transformation
  df <- df %>%
    dplyr::select(.data$model, .data$x, .data$y, .data$controls, .data$subsets) %>%
    dplyr::arrange(.data$model, .data$x, .data$y, .data$controls, .data$subsets) %>%
    dplyr::mutate(start = "raw data") %>%
    dplyr::select(start, dplyr::everything()) %>%
    dplyr::mutate(x = paste0(.data$x, " & ", .data$model),
                  y = paste0(.data$y, " & ", .data$x),
                  controls = paste0(.data$controls, " & ", .data$y),
                  subsets = paste0(.data$subsets, " & ", .data$controls))

  # Create edges
  edges_level1 <- df %>%
    dplyr::select(.data$start, .data$model) %>%
    dplyr::rename(from = .data$start, to = .data$model) %>%
    unique %>%
    dplyr::mutate(decisions = "model")
  edges_level2 <- df %>%
    dplyr::select(.data$model, .data$x) %>%
    dplyr::rename(from = .data$model, to = .data$x) %>%
    unique %>%
    dplyr::mutate(decisions = "independent variable")
  edges_level3 <- df %>%
    dplyr::select(.data$x, .data$y) %>%
    dplyr::rename(from = .data$x, to = .data$y) %>%
    unique %>%
    dplyr::mutate(decisions = "dependent variable")
  edges_level4 = df %>%
    dplyr::select(.data$y, .data$controls) %>%
    dplyr::rename(from = .data$y, to = .data$controls) %>%
    dplyr::mutate(decisions = "control variables")
  edges_level5 <- df %>%
    dplyr::select(.data$controls, .data$subsets) %>%
    dplyr::rename(from = .data$controls, to = .data$subsets) %>%
    dplyr::mutate(decisions = "subsets")

  # Combine edges
  edge_list <- rbind(edges_level1,
                     edges_level2,
                     edges_level3,
                     edges_level4,
                     edges_level5)

  # Plot edges
  plot <- edge_list %>%
    graph_from_data_frame %>%
    ggraph::ggraph(layout = 'dendrogram',
                   circular = FALSE) +
    ggraph::geom_edge_diagonal() +
    theme_void()

  # Check if legend should be plotted
  if(isTRUE(legend)) {
    plot <- plot +
      ggraph::geom_edge_diagonal(aes(color = .data$decisions)) +
      ggraph::scale_edge_color_brewer(palette = "Blues")
  }

  # Check if labels should be plotted
  if(isTRUE(label)) {
    plot <- plot +
      ggraph::geom_node_text(aes(label = .data$name,
                                 filter = .data$leaf),
                     angle=90 ,
                     hjust=1,
                     nudge_y = -0.10) +
      ylim(-5, NA)
  }

  return(plot)

}


