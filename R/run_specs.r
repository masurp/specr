#' Run specification curve analysis
#'
#' @param specs
#' @param fun_model
#' @param fun_fit
#'
#' @return
#' @export
#'
#' @examples
run_specs <- function(specs,
                      fun_model,
                      fun_fit) {
  # dependencies
  require(dplyr)
  require(furrr)

  future_map(1:nrow(specs), ~
               fun_fit(
                 fun_model(independent = specs$independent[.x],
                           dependent = specs$dependent[.x],
                           control = specs$control[.x]),
                 subset = specs$subset[.x],
                 transform = specs$transform[.x])) %>%
    map(~data.frame(x = .x)) %>%
    map(rownames_to_column) %>%
    map_df(~spread(.x, rowname, x)) %>%
    as_tibble()

}
