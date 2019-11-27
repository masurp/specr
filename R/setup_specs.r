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
#' # Setup results frame with specifications
#' setup_specs(x = c("tv_use", "smartphone_use"),
#'             y = c("depression", "loneliness"),
#'             model = c("lm", "glm"))
#'
#'
setup_specs <- function(x,
                        y,
                        model = "glm",
                        controls = "") {
  # dependencies
  require(dplyr)
  require(purrr)

  # create controls variables
  if (!is.na(controls)) {
    controls <- list(controls %>%
                       paste(collapse = " + "),
                     map(1:length(controls),
                         ~ controls[[.x]]), "") %>%
      unlist
  }
  # Expand to all possible combinations
  expand.grid(x = x,
              y = y,
              model = model,
              controls = controls) %>%
    tbl_df %>%
    mutate_all(as.character)
}
