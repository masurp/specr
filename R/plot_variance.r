#' Plot variance components of the specification curve
#'
#' @param df a data frame containing the choices and results of each specification (resulting from \code{run_specs}).
#' @param palette see "palette" within ggplot2. Defaults to "Set3".
#'
#' @return
#' @export
#'
#' @examples
plot_variance <- function(df,
                          palette = "Set3") {

  # dependencies
  require(ggplot2)

  variance_specs(df) %>%
    ggplot(aes(x = "",
               y = percent,
               fill = grp)) +
    geom_bar(stat = "identity", color = "white") +
    scale_fill_brewer(palette = palette) +
    theme_minimal() +
    coord_flip() +
    theme(axis.text = element_text(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          axis.line.x = element_line(colour = "black")) +
    labs(x = "", y = "proportion of variance", fill = "analytical choices")
}
