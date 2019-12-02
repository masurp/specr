
# Create regression formula based on setup_specs
create_formula <- function(x, y, controls, ...) {
  if (controls == "") controls <- 1
  paste(y, "~", x, "+", controls)
}

# run individual specification
run_spec <- function(specs, df) {

  # dependencies
  require(dplyr)
  require(purrr)

  specs %>%
    mutate(formula = pmap(., create_formula)) %>%
    tidyr::unnest(formula) %>%
    mutate(res = map2(model, formula, ~ do.call(.x, list(data = df, formula = .y)))) %>%
    mutate(coefs = map(res, broom::tidy),
           obs = map(res, nobs)) %>%
    tidyr::unnest(coefs) %>%
    tidyr::unnest(obs) %>%
    filter(term == x) %>%
    select(-formula, -res, -term)
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


format_results <- function(df, prob) {

  # dependencies
  require(dplyr)

  df %>%
    mutate(specifications = 1:n(),
           ll = estimate - qnorm(prob)*std.error,
           ul = estimate + qnorm(prob)*std.error,
           color = case_when(ll > 0 ~ "#377eb8",
                             ul < 0 ~ "#e41a1c",
                             TRUE ~ "grey"))
}

