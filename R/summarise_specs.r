#' Summarise specifications
#'
#' This function allows to inspect results of the specification curves by returning a comparatively simple summary of the results. This summary can be produced for various specific analytical choices and customized summary functions.
#'
#' @param df a data frame resulting from \code{run_specs()}.
#' @param ... one or more grouping variables (e.g., subsets, controls,...) that denote the available analytical choices.
#' @param var which variable should be evaluated? Defaults to estimate (the effect sizes computed by [run_specs()]).
#' @param stats named vector or named list of summary functions (individually defined summary functions can included). If it is not named, placeholders (e.g., "fn1") will be used as column names.
#'
#' @return a [tibble][tibble::tibble-package].
#'
#' @export
#'
#' @examples
#' # Run specification curve analysis
#' results <- run_specs(df = example_data,
#'                      y = c("y1", "y2"),
#'                      x = c("x1", "x2"),
#'                      model = c("lm"),
#'                      controls = c("c1", "c2"),
#'                      subsets = list(group1 = unique(example_data$group1),
#'                                     group2 = unique(example_data$group2)))
#'
#' # overall summary
#' summarise_specs(results)
#'
#' # Summary of specific analytical choices
#' summarise_specs(results,    # data frame
#'                 x, y)       # analytical choices
#'
#' # Summary of other parameters across several analytical choices
#' summarise_specs(results,
#'                 subsets, controls,
#'                 var = p.value,
#'                 stats = list(median = median,
#'                              min = min,
#'                              max = max))
#'
#' # Unnamed vector instead of named list passed to `stats`
#' summarise_specs(results,
#'                 controls,
#'                 stats = c(mean, median))
#'
#' @seealso [plot_summary()] to visually investigate the affect of analytical choices.
summarise_specs <- function(df,
                            ...,
                            var = .data$estimate,
                            stats = list(median = median, mad = mad, min = min, max = max,
                                         q25 = function(x) quantile(x, prob = .25),
                                         q75 = function(x) quantile(x, prob = .75))) {


  group_var <- enquos(...)

  # internal function
  summary_specs <- function(df) {

    var <- enquo(var)

    df %>%
      dplyr::summarize_at(vars(!! var), stats)
  }

  # is grouping variable provided?
  if (length(group_var) == 0) {

    dplyr::bind_cols(
       df %>%
         summary_specs,
       df %>%
         dplyr::summarize(obs = median(.data$obs))
     )

  } else {

    dplyr::left_join(
      df %>%
        dplyr::group_by(!!! group_var) %>%
        summary_specs,
      df %>%
        dplyr::group_by(!!! group_var) %>%
        dplyr::summarize(obs = median(.data$obs)),
      by = names_from_dots(...)
    )
  }
}


