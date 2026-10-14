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
          ~utils::combn(covariate, .x, FUN = list))
      ) %>%
        purrr::map(~paste(.x, collapse = " + "))
    ) %>%
      unlist
  }
}

expand_covariate_simple <- function(covariate) {

  if (!rlang::is_null(covariate) & length(covariate) == 1) {

    list(1, covariate) %>%
      unlist

  } else if (!rlang::is_null(covariate) & length(covariate) > 1) {

     list(1,
          purrr::map(1:length(covariate), ~covariate[[.x]]),
          covariate %>% paste(collapse = " + ")) %>%
      unlist

  } else {
    "1"
  }
}

# Rows of the data that belong to the subset of a specification. `spec` is one
# row of the specification grid; `sub_cols` are its subset columns (factors, NA = all)
spec_subset_mask <- function(spec, data, sub_cols) {

  keep <- rep(TRUE, nrow(data))

  for (column in sub_cols) {
    value <- spec[[column]]
    if (is.na(value)) next
    keep <- keep & !is.na(data[[column]]) & data[[column]] == as.character(value)
  }

  keep
}

# Function to determine the method of parameter extraction. Note:
# lean_model_function() (R/infer_curve.R) reads `f` and `fun1` from the
# environment of the returned function, so keep these names
tidy_model <- function(f, fun1, fun2) {

  # Evaluate the arguments now, so that the returned function does not keep the
  # calling environment (e.g., of setup() with the data) alive
  force(f)
  force(fun1)
  force(fun2)

  function(...) {
    fit <- do.call(f, args=list(...))
    fit1 <- fun1(fit)

    if(is.null(fun2)) {
      fit1 <- fit1 %>% mutate(fit_nobs = tryCatch(stats::nobs(fit), error = function(e) NA))
      return(fit1)
    } else {
    fit2 <- fun2(fit)
    colnames(fit2) <- paste("fit", colnames(fit2), sep = "_")
    fit <- bind_cols(fit1, fit2)
    return(fit)
    }
  }
}

# formats results (`group` and `var` are quosures)
format_results <- function(df, var, group, null = 0, desc = FALSE) {

  if (isFALSE(desc)) {
    df <- df %>%
      dplyr::arrange(!! group, !! var)
  } else {
    df <- df %>%
      dplyr::arrange(!! group, dplyr::desc(!! var))
  }

  # create rank variable and color significance (grey if there are no
  # confidence intervals). seq_len(n()) instead of 1:nrow(df), because results
  # can contain a column named `df` (e.g., lmerTest models, GitHub issue #48)
  if (all(c("conf.low", "conf.high") %in% names(df))) {
    df <- df %>%
      dplyr::mutate(specifications = seq_len(dplyr::n()),
                    color = case_when(conf.low > !! null ~ "#377eb8",
                                      conf.high < !! null ~ "#e41a1c",
                                      TRUE ~ "darkgrey"))
  } else {
    df <- df %>%
      dplyr::mutate(specifications = seq_len(dplyr::n()),
                    color = "darkgrey")
  }

  return(df)
}

# get names from dots
names_from_dots <- function(...) {

  sapply(substitute(list(...))[-1], deparse)

}


