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

# Function to determine the method of parameter extraction
tidy_model <- function(f, fun1, fun2) {
  function(...) {
    fit <- do.call(f, args=list(...))
    fit1 <- fun1(fit)

    if(is.null(fun2)) {
      fit1 <- fit1 %>% mutate(fit_nobs = broom::glance(fit)$nobs)
      return(fit1)
    } else {
    fit2 <- fun2(fit)
    colnames(fit2) <- paste("fit", colnames(fit2), sep = "_")
    fit <- bind_cols(fit1, fit2)
    return(fit)
    }
  }
}

# formats results
format_results <- function(df, var, group, null = 0, desc = FALSE) {

  if(is.null(group)) {
    if (isFALSE(desc)) {
      df <- df %>%
        dplyr::arrange(!! var)
    } else {
      df <- df %>%
        dplyr::arrange(desc(!! var))
    }
  } else {
    if (isFALSE(desc)) {
      df <- df %>%
        dplyr::arrange(!! group, !! var)
    } else {
      df <- df %>%
        dplyr::arrange(!! group, desc(!! var))
    }
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


