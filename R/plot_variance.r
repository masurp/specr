#' Plot variance components of the specification curve
#'
#' @param df a data frame containing the choices and results of each specification (resulting from \code{run_specs}).
#'
#' @return
#' @export
#'
#' @examples
plot_variance <- function(df) {

  # dependencies
  require(ggplot2)

  variance_specs(df) %>%
    ggplot(aes(x = grp,
               y = percent)) +
    geom_bar(stat = "identity", fill = "#377eb8") +
    theme_minimal() +
    theme(axis.text = element_text(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          axis.line.x = element_line(colour = "black")) +
    labs(x = "", y = "proportion of variance", fill = "analytical choices")
}
