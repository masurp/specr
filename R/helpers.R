
# create regression formula based on setup_specs
create_formula <- function(x, y, controls, ...) {

  if (controls == "no covariates") controls <- 1
  paste(y, "~", x, "+", controls)

}

# run individual specification
run_spec <- function(specs, df, conf.level, keep.results = FALSE) {

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
                  obs = map(.data$res, nobs)) %>%
    tidyr::unnest(.data$coefs) %>%
    tidyr::unnest(.data$obs) %>%
    dplyr::filter(.data$term == .data$x) %>%
    dplyr::select(-.data$formula, -.data$term)

  if (isFALSE(keep.results)) {
    results <- results %>%
      dplyr::select(-.data$res)
  }

  return(results)
}

# creates subsets
create_subsets <- function(df, subsets) {

  subsets %>%
    stack %>%
    pmap(~ dplyr::filter(df, get(as.character(..2)) == ..1) %>%
    dplyr::mutate(filter = paste(..2, "=", ..1)))
}


# formats results
format_results <- function(df, null = 0, desc = FALSE) {

  # rank specs
  if (isFALSE(desc)) {
    df <- df %>%
      dplyr::arrange(.data$estimate)
  } else {
    df <- df %>%
      dplyr::arrange(desc(.data$estimate))
  }

  # create rank variable and color significance
  df <- df %>%
    dplyr::mutate(specifications = 1:n(),
                  color = case_when(conf.low > null ~ "#377eb8",
                                    conf.high < null ~ "#e41a1c",
                                    TRUE ~ "darkgrey"))
  return(df)
}

# get names from dots
names_from_dots <- function(...) {

  sapply(substitute(list(...))[-1], deparse)

}
