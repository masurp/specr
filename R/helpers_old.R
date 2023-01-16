
# create regression formula based on setup_specs
create_formula <- function(x,
                           y,
                           controls,
                           ...) {

  if (controls == "no covariates") controls <- 1
  paste(y, "~", x, "+", controls)

}

# run specifications
run_spec <- function(specs,
                     df,
                     conf.level,
                     keep.results = FALSE) {
  results <- specs %>%
    dplyr::mutate(formula = pmap(specs, create_formula)) %>%
    tidyr::unnest(formula) %>%
    dplyr::mutate(res = map2(.data$model,
                             formula,
                             ~ do.call(.x, list(data = df,
                                                formula = .y)))) %>%
    dplyr::mutate(coefs = map(.data$res,
                              broom::tidy,
                              conf.int = TRUE,
                              conf.level = conf.level),
                  fit = map(.data$res, broom::glance)) %>%
    tidyr::unnest(.data$coefs) %>%
    tidyr::unnest(.data$fit, names_sep = "_")

  if("op" %in% names(results)) {
    results <- results %>%
      dplyr::filter(.data$term == paste(.data$y, "~", .data$x))
  } else {
    results <- results %>%
      dplyr::filter(.data$term == .data$x)
    }

  results <- results %>%
    dplyr::select(-.data$formula, -.data$term)

  if (isFALSE(keep.results)) {
    results <- results %>%
      dplyr::select(-.data$res)
  }

  return(results)
}

# creates subsets
create_subsets <- function(df,
                           subsets) {

  subsets %>%
    stack %>%
    pmap(~ dplyr::filter(df, get(as.character(..2)) == ..1) %>%
    dplyr::mutate(filter = paste(..2, "=", ..1)))
}






