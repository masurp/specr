
# create regression formula based on setup_specs
create_formula <- function(x, y, controls, ...) {

  if (controls == "no covariates") controls <- 1
  paste(y, "~", x, "+", controls)

}

# run individual specification
run_spec <- function(specs, df, conf.level, keep.results = FALSE) {

  require(purrr)

  results <- specs %>%
    dplyr::mutate(formula = pmap(., create_formula)) %>%
    tidyr::unnest(formula) %>%
    dplyr::mutate(res = map2(model, formula, ~ do.call(.x, list(data = df, formula = .y)))) %>%
    dplyr::mutate(coefs = map(res, broom::tidy, conf.int = TRUE, conf.level = conf.level),
                  obs = map(res, nobs)) %>%
    tidyr::unnest(coefs) %>%
    tidyr::unnest(obs) %>%
    dplyr::filter(term == x) %>%
    dplyr::select(-formula, -term)

  if (isFALSE(keep.results)) {
    results <- results %>%
      dplyr::select(-res)
  }

  return(results)
}

# creates subsets
create_subsets <- function(df, subsets) {

  subsets %>%
    stack %>%
    purrr::pmap(~ dplyr::filter(df, get(as.character(..2)) == ..1) %>%
                  dplyr::mutate(filter = paste(..2, "=", ..1)))
}


# formats results
format_results <- function(df, null = 0, desc = FALSE) {

  # rank specs
  if (isFALSE(desc)) {
    df <- df %>%
      dplyr::arrange(estimate)
  } else {
    df <- df %>%
      dplyr::arrange(desc(estimate))
  }

  # create rank variable and color significance
  df <- df %>%
    dplyr::mutate(specifications = 1:n(),
                  color = case_when(conf.low > null ~ "#377eb8",
                                    conf.high < null ~ "#e41a1c",
                                    TRUE ~ "darkgrey"))
  return(df)
}

