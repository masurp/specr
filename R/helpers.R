# Exand covariates
expand_covariate <- function(covariate) {

  if(is.null(covariate)) {
    "1"
  } else {
    list(
      "1",
      do.call(
        "c",
        purrr::map(
          seq_along(covariate),
          ~combn(covariate, .x, FUN = list))
      ) %>%
        purrr::map(~paste(.x, collapse = " + "))
    ) %>%
      unlist
  }
}

# Function to determine the method of parameter extraction
tidy_model <- function(f, fun) {
  function(...) {
    fit <- do.call(f, args=list(...))
    fit1 <- fun(fit)
    fit2 <- broom::glance(fit)
    colnames(fit2) <- paste("fit", colnames(fit2), sep = "_")
    fit <- bind_cols(fit1, fit2)
    return(fit)
  }
}

# formats results
format_results <- function(df, var, null = 0, desc = FALSE) {

  # rank specs
  if (isFALSE(desc)) {
    df <- df %>%
      dplyr::arrange(!! var)
  } else {
    df <- df %>%
      dplyr::arrange(desc(!! var))
  }

  # create rank variable and color significance
  df <- df %>%
    dplyr::mutate(specifications = 1:nrow(df),
                  color = case_when(conf.low > null ~ "#377eb8",
                                    conf.high < null ~ "#e41a1c",
                                    TRUE ~ "darkgrey"))
  return(df)
}

# get names from dots
names_from_dots <- function(...) {

  sapply(substitute(list(...))[-1], deparse)

}


