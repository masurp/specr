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

format_choice_panel <- function(df, choices) {

  value <- key <- NULL

  choice_panel <- df %>%
    tidyr::gather(key, value, choices) %>%
    dplyr::mutate(key = factor(.data$key, levels = choices))

  if (!"controls" %in% choices) {
    return(choice_panel)
  }

  controls_rows <- choice_panel[as.character(choice_panel$key) == "controls", , drop = FALSE]

  if (nrow(controls_rows) == 0) {
    return(choice_panel)
  }

  other_rows <- choice_panel[as.character(choice_panel$key) != "controls", , drop = FALSE]

  controls_values <- as.character(controls_rows$value)
  individual_controls <- controls_values[
    !is.na(controls_values) &
      controls_values != "no covariates" &
      controls_values != "all covariates" &
      !grepl("\\+", controls_values)
  ]
  individual_controls <- unique(trimws(individual_controls))
  individual_controls <- individual_controls[nzchar(individual_controls)]

  split_control_values <- lapply(controls_values, function(value) {
    if (is.na(value)) {
      return(NA_character_)
    }

    value <- trimws(value)

    if (value == "all covariates" && length(individual_controls) > 0) {
      return(individual_controls)
    }

    if (grepl("\\+", value)) {
      split_values <- trimws(strsplit(value, "\\+")[[1]])
      return(split_values[nzchar(split_values)])
    }

    value
  })

  row_indices <- rep(seq_len(nrow(controls_rows)), lengths(split_control_values))
  controls_rows <- controls_rows[row_indices, , drop = FALSE]
  controls_rows$value <- unlist(split_control_values, use.names = FALSE)

  dplyr::bind_rows(other_rows, controls_rows)
}

# get names from dots
names_from_dots <- function(...) {

  sapply(substitute(list(...))[-1], deparse)

}

