#' Run specification curve analysis
#'
#' This is the central function of the package. It can be used to run the specification curve analyses. It takes the data frame and vectors for analytical choices related to the dependent variable, the independent variable, the type of models that should be estimated, the set of covariates that should be included (none, each individually, and all together), as well as a named list of potential subsets. It returns a "result frame" which includes relevant statistics for each model as well as the analytical choices as factorial variables.
#'
#' @param df a data frame that includes all relevant variables
#' @param y a vector of the dependent variables
#' @param x a vevtor of the dependent variables
#' @param model a vector of the type of models that should be estimated.
#' @param controls a vector of the control variables that should be included. Defaults to none.
#' @param subsets a list that includes named vectors
#' @param conf.level the confidence level to use for the confidence interval. Must be strictly greater than 0 and less than 1. Defaults to 0.95, which corresponds to a 95 percent confidence interval.
#'
#' @return
#' @export
#'
#' @examples
#' # run specification curve analysis
#' results <- run_specs(df = example_data,
#'                      y = c("y1", "y2"),
#'                      x = c("x1", "x2"),
#'                      model = c("lm"),
#'                      controls = c("c1", "c2"),
#'                      subsets = list(group1 = unique(example_data$group1),
#'                                     group2 = unique(example_data$group2)))
#'
#' # Check results frame
#' results
run_specs <- function(df, y, x, model, controls = NULL, subsets = NULL, conf.level = 0.95, keep.results = FALSE) {

  # dependencies
  require(dplyr, quietly = TRUE)
  require(purrr, quietly = TRUE)



  specs <- setup_specs(y = y, x = x, model = model, controls = controls)

  if (!is.null(subsets)) {

  subsets = map(subsets, as.character)

  # Create subsets and full data set, but no combination
  df_list <- create_subsets(df, subsets)
  df_list[[length(df_list)+1]] <- df %>% mutate(filter = "all")

  if (length(subsets) > 1) {

  suppressMessages({
  df_comb <- subsets %>%
    cross %>%
    map(~ create_subsets(subsets = .x, df = df) %>%
          map(~select(.x, -filter)) %>%
          reduce(inner_join) %>%
          mutate(filter = paste(names(.x), .x, collapse = " & ", sep = " = ")))

  df_all <- append(df_list, df_comb)
  })

  } else {

  df_all <- df_list

  }

  map_df(df_all, ~ run_spec(specs, .x, conf.level = conf.level, keep.results = keep.results) %>%
           mutate(subsets = unique(.x$filter)))

  } else {

  run_spec(specs, df, conf.level = conf.level, keep.results = keep.results) %>%
    mutate(subsets = "all")

  }

}
