#' Set up specifications
#'
#' This function creates a tibble that includes all possible specifications based on different dependent and independent variables, different types of models, and different control variables. This function can be used to check the combinations of analytical choices. Otherwise, the function is called within [run_specs()].
#'
#' @param x a vector of the independent variables
#' @param y a vector of the dependent variables
#' @param model a vector of the type of models that should be estimated
#' @param controls a vector of the control variables that should be included. Defaults to none.
#'
#' @return a [tibble][tibble::tibble-package] that includes all combinations of determined analytical choices.
#' @export
#'
#' @examples
#' setup_specs(y = c("y1"),
#'             x = c("x1", "x2"),
#'             model = c("lm"),
#'             controls = c("c1", "c2"))
#'
#'@seealso [run_specs()] to run the specification curve analysis.
setup_specs <- function(x,
                        y,
                        model,
                        controls = NULL) {

  # create controls variables
  if (!rlang::is_null(controls)) {
    controls <- list(controls %>%
                     paste(collapse = " + "),
                     purrr::map(1:length(controls),
                               ~ controls[[.x]]), "no covariates") %>%
      unlist

  } else {

    controls <- "no covariates"

  }
  # Expand to all possible combinations
  expand.grid(x = x,
              y = y,
              model = model,
              controls = controls) %>%
    dplyr::mutate_all(as.character) %>%
    as_tibble
}

