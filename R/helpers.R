
# create regression formula based on setup_specs
create_formula <- function(x,
                           y,
                           controls,
                           ...) {

  if (controls == "no covariates") controls <- 1
  paste(y, "~", x, "+", controls)

}

# run specifications
run_spec <- function(specs,
                     df,
                     conf.level,
                     keep.results = FALSE) {

  results <- specs %>%
    dplyr::mutate(formula = purrr::pmap_chr(specs, create_formula))

  # Compute over specs (possibly in parallel)
  results$res <- vector("list", nrow(results))
  results$res <- foreach(i = 1:nrow(results)) %dopar%
    do.call(
      what = results[i,"model", drop = TRUE],
      args = list(formula = results[i,"formula", drop = TRUE], data = df)
    )

  results <- results %>%
    dplyr::mutate(coefs = map(.data$res,
                              broom::tidy,
                              conf.int = TRUE,
                              conf.level = conf.level),
                  fit = map(.data$res, broom::glance)) %>%
    tidyr::unnest(.data$coefs) %>%
    tidyr::unnest(.data$fit, names_sep = "_")

  if("op" %in% names(results)) {
    results <- results %>%
      dplyr::filter(.data$term == paste(.data$y, "~", .data$x))
  } else {
    results <- results %>%
      dplyr::filter(.data$term == .data$x)
    }

  results <- results %>%
    dplyr::select(-.data$formula, -.data$term)

  if (isFALSE(keep.results)) {
    results <- results %>%
      dplyr::select(-.data$res)
  }

  return(results)
}

# creates subsets
create_subsets <- function(df,
                           subsets) {

  subsets %>%
    stack %>%
    pmap(~ dplyr::filter(df, get(as.character(..2)) == ..1) %>%
    dplyr::mutate(filter = paste(..2, "=", ..1)))
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

