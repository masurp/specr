# Exand covariates
expand_covariate <- function(covariate) {

  if(is.null(covariate)) {
    "1"
  } else {
    list(
      "1",
      do.call(
        "c",
        map(
          seq_along(covariate),
          ~combn(covariate, .x, FUN = list))
      ) %>%
        map(~paste(.x, collapse = " + "))
    ) %>%
      unlist
  }
}

# Function to determine the method of parameter extraction
tidy_model <- function(f, tidy_f) {
  function(...) {
    fit = do.call(f, args=list(...))
    tidy_f(fit) %>%
      slice(2) %>%
      mutate(fit_nobs = nobs(fit))
  }
}

