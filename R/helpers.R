# Create regression formula based on setup_specs
create_formula <- function(x, y, controls, ...) {
  if (controls == "") controls <- 1
  as.formula(paste(y, "~", x, "+", controls))
}

# run individual specification
run_spec <- function(specs, df) {
  specs %>%
    mutate(formula = pmap(., create_formula)) %>%
    mutate(res = map2(model, formula, ~ do.call(.x, list(data = df, formula = .y)))) %>%
    mutate(coefs = map(res, broom::tidy)) %>%
    tidyr::unnest(coefs) %>%
    select(-formula, -res, -term)
}

# create subsets
create_subsets <- function(df, subsets) {
  subsets %>%
    map_dfc(~.x) %>%
    tidyr::gather(k, v) %>%
    pmap(~ filter(df, get(..1) == ..2) %>%
      mutate(filter = paste(..1, "=", ..2)))
}
