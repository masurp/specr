#' Plot how analytical choices affect results
#'
#' @param df a data frame containing the choices and results of each specification (resulting from \code{run_specs}).
#' @param desc logical value indicating whether the curve should the arranged in a descending order. Defaults to FALSE.
#'
#' @return
#' @export
#'
#' @examples
plot_choices <- function(df,
                         desc = FALSE) {

  require(ggplot2)
  require(dplyr)

  # rank specs
  if (isFALSE(desc)) {
    df <- df %>%
      arrange(estimate)
  } else {
    df <- df %>%
      arrange(desc(estimate))
  }

  df %>%
    mutate(specifications = 1:n()) %>%
    tidyr::gather(key, value, -estimate, -std.error, -statistic, -p.value, -specifications) %>%
    ggplot(aes(x = specifications,
               y = value,
               color = p.value < .05)) +
    geom_point(aes(x = specifications,
                   y = value),
               shape = 124,
               size = 3.35) +
    theme_minimal() +
    facet_wrap(~key, scales = "free_y", ncol = 1) +
    theme(strip.text = element_blank(),
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          panel.spacing = unit(.75, "lines"),
          axis.text = element_text(colour = "black"))

}
