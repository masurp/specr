
# Create regression formula based on setup_specs
create_formula <- function(x, y, controls, ...) {
  if (controls == "no covariates") controls <- 1
  paste(y, "~", x, "+", controls)
}

# run individual specification
run_spec <- function(specs, df, conf.level, keep.results = FALSE) {

  # dependencies
  require(dplyr)
  require(purrr)

  results <- specs %>%
    mutate(formula = pmap(., create_formula)) %>%
    tidyr::unnest(formula) %>%
    mutate(res = map2(model, formula, ~ do.call(.x, list(data = df, formula = .y)))) %>%
    mutate(coefs = map(res, broom::tidy, conf.int = TRUE, conf.level = conf.level),
           obs = map(res, nobs)) %>%
    tidyr::unnest(coefs) %>%
    tidyr::unnest(obs) %>%
    filter(term == x) %>%
    select(-formula, -term)

  if (isFALSE(keep.results)) {
    results <- results %>%
      select(-res)
  }

  return(results)
}

# create subsets
create_subsets <- function(df, subsets) {

  # dependencies
  require(dplyr)

  subsets %>%
    stack %>%
    purrr::pmap(~ filter(df, get(as.character(..2)) == ..1) %>%
                  mutate(filter = paste(..2, "=", ..1)))
}


format_results <- function(df, null = 0, desc = FALSE) {

  # dependencies
  require(dplyr)
  # rank specs
  if (isFALSE(desc)) {
    df <- df %>%
      arrange(estimate)
  } else {
    df <- df %>%
      arrange(desc(estimate))
  }
  df <- df %>%
    mutate(specifications = 1:n(),
           color = case_when(conf.low > null ~ "#377eb8",
                             conf.high < null ~ "#e41a1c",
                             TRUE ~ "darkgrey"))

  return(df)
}

