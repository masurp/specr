#' Run specification curve analysis
#'
#' @param df
#' @param y
#' @param x
#' @param model
#' @param controls
#' @param subsets
#'
#' @return
#' @export

run_specs <- function(df, y, x, model, controls, subsets) {
  specs <- setup_specs(y = y, x = x, model = model, controls = controls)
  df_list <- create_subsets(df, subsets)
  map_df(df_list, ~ run_spec(specs, .x) %>%
    mutate(subset = unique(.x$filter)))
}
