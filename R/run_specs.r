#' Run specification curve analysis
#'
#' @param df a data frame that includes all relevant variables
#' @param y a vector of the dependent variables
#' @param x a vevtor of the dependent variables
#' @param model a vector of the type of models that should be estimated
#' @param controls a vector of the control variables that should be included
#' @param subsets a list that includes named vectors
#'
#' @return
#' @export

run_specs <- function(df, y, x, model, controls, subsets) {

  specs <- setup_specs(y = y, x = x, model = model, controls = controls)
  df_list <- create_subsets(df, subsets)
  map_df(df_list, ~ run_spec(specs, .x) %>%
    mutate(subset = unique(.x$filter)))

}
