#' Plot specification curve
#'
#' @param df data frame resulting from run_specs().
#' @param labels labels for the two parts of the plot
#' @param rel_heights vector indicating the relative heights of the plot
#' @param ci logical value indicating whether confidence intervals should be plotted.
#'
#' @return
#' @export
#'
#' @examples
#'
plot_specs <- function(df,
                       labels = c("A", "B"),
                       rel_heights = c(2, 2.5),
                       ci = TRUE) {

  # dependencies
  require(dplyr)
  require(ggplot2)
  require(cowplot)

  # rank specs and estiamte CIs
  df <- df %>%
    arrange(estimate) %>%
    mutate(rank = 1:n()) %>%
    mutate(ll = estimate - 1.96*std.error,
           ul = estimate + 1.96*std.error)

  p1 <- plot_curve(df, ci)
  p2 <- plot_choices(df)


  plot_grid(p1, p2,
            labels = labels,
            align = "v",
            axis = "b",
            rel_heights = rel_heights,
            ncol = 1)
}


# Internal functions
## Plot 1 - Specification curve
plot_curve <- function(df,
                       ci){


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
    papaja::theme_apa() +
    theme(strip.text = element_blank(),
          axis.line.y = element_line("black", size = .5),
          axis.line.x = element_line("black", size = .5),
          axis.text = element_text(colour = "black"),
          legend.position = "none") +
    labs(x = "", y = "")

  # add CIs if necessary
  if (isTRUE(ci)) {
  plot <- plot +
    geom_pointrange(alpha = 0.5,
                    color = "grey",
                    size = .6,
                    fatten = 1)
  }

  return(plot)
}

## Plot 2 - Analytical choices
plot_choices <- function(df) {

  df %>%
    gather(key, value, -estimate, -std.error, -statistic, -p.value, -rank, -ll, -ul) %>%
    ggplot(aes(x = rank,
               y = value,
               color = p.value < .05)) +
    geom_point(aes(x = rank,
                   y = value),
               shape = 124,
               size = 3.35) +
    scale_color_manual(values = c("#FF0000", "#000000")) +
    papaja::theme_apa() +
    facet_wrap(~key, scales = "free_y", ncol = 1) +
    theme(strip.text = element_blank(),
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          panel.spacing = unit(.75, "lines"),
          axis.text = element_text(colour = "black")) +
    labs(x = "", y = "")

}

