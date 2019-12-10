#' Summarise the specification curve results
#'
#' This function allows to inspect results of the specification curves by returning a comparatively simple summary of the results. These results can be returned for specific analytical choices.
#'
#' @param df a data frame containing the choices and results of each specification (resulting from \code{run_specs}).
#' @param var which variable should be evaluated? Defaults to estimate (the effect sizes computed by \code{run_specs}). No need to use closures.
#' @param group a grouping factor (e.g., "subsets"). Several grouping variables can be passed. Defaults to NULL.

#'
#' @return
#' @export
#'
#' @examples
summarise_specs <- function(df,
                            var = estimate,
                            group = NULL) {

  require(dplyr)


  # Internal function
  summary_specs <- function(df) {

    var <- enquo(var)

    df %>%
      summarize(mean = mean(!! var),
                sd = sd(!! var),
                median = median(!! var),
                mad = mad(!! var),
                min = min(!! var),
                max = max(!! var),
                obs_med = median(obs))
  }


  if (is_null(group)) {

    df %>%
      summary_specs


  } else {

    group <- lapply(group, as.symbol)

    df %>%
      group_by_(.dots = group) %>%
      summary_specs
  }

}
