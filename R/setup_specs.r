#' Create result frame with all specifications
#'
#' This function creates a data frame that includes all possible specifications based on different dependent and independent variables, different types of models, and different control variables. This function can be used to check the combinations of analytical choices. Otherwise, the function is called with \code{run_specs()}.
#'
#' @param x a vector of the independent variables
#' @param y a vector of the dependent variables
#' @param model a vector of the type of models that should be estimated
#' @param controls a vector of the control variables that should be included. Defaults to none.
#'
#' @return
#' @export
#'
#' @examples
#' setup_specs(y = c("y1"),               # We choose only one dependent variale
#'             x = c("x1", "x2"),         # We are not sure which independent variable is better
#'             model = c("lm"),           # We only estimate one type of model (linear model)
#'             controls = c("c1", "c2"))  # We include two control variable
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
    dplyr::mutate_all(as.character)
}

