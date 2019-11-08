#' Create result frame with specifications
#'
#' @param independent Enter independent variable as vector.
#' @param dependent
#' @param results
#' @param control
#' @param subset
#' @param transform
#'
#' @return
#' @export
#'
#' @examples
#' # Create variable for subsetting
#' gender <- c(rep(c("male", "female"), 10))
#'
#' # Setup results frame with specifications
#' setup_specs(independent = c("tv_use", "smartphone_use"),
#'             dependent = c("depression", "loneliness"),
#'             control = c("self_esteem", "number_friends"),
#'             subset = gender)
#'
#'
setup_specs <- function(independent,
                        dependent,
                        control = NA,
                        subset = NA,
                        transform = NA) {
  # dependencies
  require(dplyr)


  # create control variables
  if (!is.na(control)) {

    control <- list(control %>%
                      paste(collapse=" + "),
                    map(1:length(control),
                        ~ control[[.x]]), "") %>%
      unlist

  }

  # create subsets
  if (!is.na(subset)) {

    subset <- c(levels(as.factor(subset)), "all")


  }
  # Expand to all possible combinations
  expand.grid(independent = independent,
              dependent = dependent,
              control = control,
              subset = subset,
              transform = transform) %>%
    tbl_df
}
