#' Summarise the specification curve results
#'
#' This function allows to inspect results of the specification curves by returning a comparatively simple summary of the results. These results can be returned for specific analytical choices.
#'
#' @param df a data frame containing the choices and results of each specification (resulting from \code{run_specs}).
#' @param group a grouping factor (e.g., "subsets"). Defaults to NULL.
#'
#' @return
#' @export
#'
#' @examples
summarise_specs <- function(df,
                            group = NULL) {

  require(dplyr)

  # Internal function
  summary_specs <- function(df) {

    df %>%
    summarize(mean = mean(estimate),
              sd = sd(estimate),
              median = median(estimate),
              mad = mad(estimate),
              min = min(estimate),
              max = max(estimate),
              obs_med = median(obs))
  }


  if (is_null(group)) {

  df %>%
      summary_specs


  } else {
    group <- as.name(group)

    df %>%
      group_by(!!group) %>%
      summary_specs
  }

}
