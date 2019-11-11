#' Create result frame with specifications
#'
#' @param x Enter x variable as vector.
#' @param y
#' @param results
#' @param controls
#'
#' @return
#' @export
#'
#' @examples
#' # Setup results frame with specifications
#' setup_specs(x = c("tv_use", "smartphone_use"),
#'             y = c("depression", "loneliness"),
#'             controls = c("self_esteem", "number_friends"))
#'
#'
setup_specs <- function(x,
                        y,
                        model = "glm",
                        controls = NA) {
  # dependencies
  require(dplyr)
  require(purrr)

  # create controls variables
  if (!is.na(controls)) {
    controls <- list(controls %>%
                       paste(collapse = " + "),
                     map(1:length(controls),
                         ~ controls[[.x]]),
                     "") %>%
      unlist

  }

  # Expand to all possible combinations
  expand.grid(x = x, y = y, model = model, controls = controls) %>%
    tbl_df %>%
    mutate_all(as.character)
}
