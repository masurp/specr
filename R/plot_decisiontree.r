#' Plot the decision tree
#'
#' This function plots a simple decision tree that can be used to understand how analytical choices produce a certain number of specifications. It is somewhat useless if the number of specifications is very high.
#'
#' @param df data frame resulting from \code{run_specs()}.
#' @param label Logical. Should labels be included? Defaults to FALSE. Produces only a reasonable plot if number of specifications is low.
#' @param legend Logical. Should specific decisions be identifiable. Defaults to FALSE.
#'
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
#' plot_decisions(results)
#'
#' # Labelled decisions tree
#' plot_decisions(results, label = TRUE)
#'
#' # Identify choices
#' plot_decisions(results, legend = TRUE)
#'
#' # All combined
#' plot_decisions(results, label = TRUE, legend = TRUE)
plot_decisiontree <- function(df,
                           label = FALSE,
                           legend = FALSE) {
  library(ggraph)
  library(igraph)

df <- df %>%
  select(model, x, y, controls, subsets) %>%
  arrange(model, x, y, controls, subsets) %>%
  mutate(start = "raw data") %>%
  select(start, everything()) %>%
  mutate(x = paste0(x, " & ", model),
         y = paste0(y, " & ", x),
         controls = paste0(controls, " & ", y),
         subsets = paste0(subsets, " & ", controls))

edges_level1 <- df %>%
  select(start, model) %>%
  rename(from=start, to=model) %>%
  unique %>%
  mutate(decisions = "model")
edges_level2 <- df %>%
  select(model, x) %>%
  rename(from=model, to = x) %>%
  unique %>%
  mutate(decisions = "independent variable")
edges_level3 <- df %>%
  select(x, y) %>%
  rename(from=x, to=y) %>%
  unique %>%
  mutate(decisions = "dependent variable")
edges_level4 = df %>%
  select(y, controls) %>%
  rename(from=y, to=controls) %>%
  mutate(decisions = "control variables")
edges_level5 <- df %>%
  select(controls, subsets) %>%
  rename(from=controls, to=subsets) %>%
  mutate(decisions = "subsets")

edge_list <- rbind(edges_level1,
                   edges_level2,
                   edges_level3,
                   edges_level4,
                   edges_level5)

plot <- edge_list %>%
  graph_from_data_frame %>%
  ggraph(layout = 'dendrogram', circular = FALSE) +
  geom_edge_diagonal() +
  #geom_node_point() +
  theme_void()

if(isTRUE(legend)) {
  plot <- plot +
    geom_edge_diagonal(aes(color = decisions))
}

if(isTRUE(label)) {
  plot <- plot +
    geom_node_text(aes(label=name, filter = leaf),
                   angle=90 , hjust=1, nudge_y = -0.10) +
    ylim(-5, NA)
}

 return(plot)

}


