#' Set up specifications
#'
#' This function creates a tibble that includes all possible specifications based the dependent and independent variables, model types, and control variables that are specified. This function simply produces a tibble of all combinations. It can be used to check the specified analytical choices. This function is called within [run_specs()], which estimates all specified models based on the data that are provided.
#'
#' @param y a vector denoting the dependent variables
#' @param x a vector denoting independent variables
#' @param model a vector denoting the model(s) that should be estimated.
#' @param controls a vector of the control variables that should be included. Defaults to NULL.
#' @param all.comb a logical value indicating what type of combinations of the control variables should be specified. Defaults to FALSE (i.e., none, all, and each individually). If this argument is set to TRUE, all possible combinations between the control variables are specified (see examples).
#'
#' @return a [tibble][tibble::tibble-package] that includes all possible specifications based on combinations of the analytical choices.
#' @export
#'
#' @examples
#' setup_specs(y = c("y1"),
#'             x = c("x1", "x2"),
#'             model = c("lm"),
#'             controls = c("c1", "c2"))
#'
#' setup_specs(y = c("y1"),
#'             x = c("x1", "x2"),
#'             model = c("lm"),
#'             controls = c("c1", "c2", "c3"),
#'             all.comb = T)
#'
#'@seealso [run_specs()] to run the specification curve analysis.
setup_specs <- function(x,
                        y,
                        model,
                        controls = NULL,
                        all.comb = FALSE) {

  # create controls variables
  if (!rlang::is_null(controls) & isFALSE(all.comb)) {

    controls <- list(controls %>%
                       paste(collapse = " + "),
                     purrr::map(1:length(controls),
                                ~ controls[[.x]]), "no covariates") %>%
      unlist

  } else if(!rlang::is_null(controls) & isTRUE(all.comb)) {

    controls <- list(do.call("c", lapply(seq_along(controls),
                                         function(i) combn(controls, i, FUN = list))) %>%
                       map(., function(x) paste(x, collapse = " + ")), "no covariates") %>%
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

