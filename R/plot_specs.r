#' Plot specification curve
#'
#' @param df data frame resulting from run_specs().
#' @param ci logical value indicating whether confidence intervals should be plotted.
#'
#' @return
#' @export
#'
#' @examples
plot_specs <- function(df,
                       ci = TRUE){

  # Rank specs and estiamte CIs
  df <- df %>%
    arrange(estimate) %>%
    mutate(rank = 1:n()) %>%
      mutate(ll = estimate - 1.96*std.error,
             ul = estimate + 1.96*std.error)

  # Create basic plot
  plot <- ggplot(df, aes(x = rank,
                         y = estimate,
                         ymin = ll,
                         ymax = ul,
                         color = p.value < .05)) +
    geom_point(aes(color = p.value < .05),
               size = .6) +
    scale_color_manual(values = c("#FF0000", "#000000")) +
    geom_hline(yintercept = 0,
               linetype = "solid") +
    papaja::theme_apa()

  # Add CIs if necessary
  if (isTRUE(ci)) {
  plot <- plot +
    geom_pointrange(alpha = 0.5,
                    color = "grey",
                    size = .6,
                    fatten = 1)
  }

  return(plot)
}
