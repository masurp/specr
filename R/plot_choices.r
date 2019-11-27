#' Plot how analytical choices affect results
#'
#' This functions plots how the analytical choices affect the obtained results (i.e., the rank within the curve). Significant results are highlighted. Further customization via ggplot2 is possible.
#'
#' @param df a data frame containing the choices and results of each specification (resulting from \code{run_specs}).
#' @param desc logical value indicating whether the curve should the arranged in a descending order. Defaults to FALSE.
#'
#' @return
#' @export
#'
#' @examples
plot_choices <- function(df,
                         prob = .975,
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

  df <- df %>%
    mutate(specifications = 1:n(),
           ll = estimate - qnorm(prob)*std.error,
           ul = estimate + qnorm(prob)*std.error,
           color = case_when(ll > 0 ~ "lightblue", ul < 0 ~ "lightred", TRUE ~ "grey"))

  df %>%
    tidyr::gather(key, value, x, y, model, controls, subset) %>%
    ggplot(aes(x = specifications,
               y = value,
               color = color)) +
    geom_point(aes(x = specifications,
                   y = value),
               shape = 124,
               size = 3.35) +
    scale_color_identity() +
    theme_minimal() +
    facet_wrap(~key, scales = "free_y", ncol = 1) +
    theme(strip.text = element_blank(),
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          panel.spacing = unit(.75, "lines"),
          axis.text = element_text(colour = "black"))

}
