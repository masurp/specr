#' Plot the decision tree
#'
#' This function plots a simple decision tree that is meant to help understanding how analytical choices produce a certain number of specifications. It is somewhat useless if the number of specifications is very high.
#'
#' @param df data frame resulting from \code{run_specs()}.
#' @param label Logical. Should labels be included? Defaults to FALSE. Produces only a reasonable plot if number of specifications is low.
#' @param legend Logical. Should specific decisions be identifiable. Defaults to FALSE.
#' @return
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
    dplyr::select(model, x, y, controls, subsets) %>%
    dplyr::arrange(model, x, y, controls, subsets) %>%
    dplyr::mutate(start = "raw data") %>%
    dplyr::select(start, dplyr::everything()) %>%
    dplyr::mutate(x = paste0(x, " & ", model),
                  y = paste0(y, " & ", x),
                  controls = paste0(controls, " & ", y),
                  subsets = paste0(subsets, " & ", controls))

  # Create edges
  edges_level1 <- df %>%
    dplyr::select(start, model) %>%
    dplyr::rename(from=start, to=model) %>%
    unique %>%
    dplyr::mutate(decisions = "model")
  edges_level2 <- df %>%
    dplyr::select(model, x) %>%
    dplyr::rename(from=model, to = x) %>%
    unique %>%
    dplyr::mutate(decisions = "independent variable")
  edges_level3 <- df %>%
    dplyr::select(x, y) %>%
    dplyr::rename(from=x, to=y) %>%
    unique %>%
    dplyr::mutate(decisions = "dependent variable")
  edges_level4 = df %>%
    dplyr::select(y, controls) %>%
    dplyr::rename(from=y, to=controls) %>%
    dplyr::mutate(decisions = "control variables")
  edges_level5 <- df %>%
    dplyr::select(controls, subsets) %>%
    dplyr::rename(from=controls, to=subsets) %>%
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
      ggraph::geom_edge_diagonal(aes(color = decisions)) +
      ggraph::scale_edge_color_brewer(palette = "Blues")
  }

  # Check if labels should be plotted
  if(isTRUE(label)) {
    plot <- plot +
      geom_node_text(aes(label=name,
                         filter = leaf),
                     angle=90 ,
                     hjust=1,
                     nudge_y = -0.10) +
      ylim(-5, NA)
  }

  return(plot)

}


