#' Run specification curve analysis
#'
#' This is the central function of the package. It can be used to run the specification curve analyses. It takes the data frame and vectors for analytical choices related to the dependent variable, the independent variable, the type of models that should be estimated, the set of covariates that should be included (none, each individually, and all together), as well as a named list of potential subsets. It returns a "result frame" which includes relevant statistics for each model as well as the analytical choices as factorial variables.
#'
#' @param df a data frame that includes all relevant variables
#' @param y a vector of the dependent variables
#' @param x a vevtor of the dependent variables
#' @param model a vector of the type of models that should be estimated.
#' @param controls a vector of the control variables that should be included
#' @param subsets a list that includes named vectors
#'
#' @return
#' @export

run_specs <- function(df, y, x, model, controls, subsets = NA) {

  specs <- setup_specs(y = y, x = x, model = model, controls = controls)

  if (!is.na(subsets)) {

  df_list <- create_subsets(df, subsets)
  map_df(df_list, ~ run_spec(specs, .x) %>%
           mutate(subset = unique(.x$filter)))

  } else {

  run_spec(specs, df)

  }

}
