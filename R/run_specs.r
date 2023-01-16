#' Estimate all specifications
#'
#' @description `r lifecycle::badge("deprecated")`
#'   This function was deprecated because the new version of specr uses different analytical framework. In this framework, you should use the function [setup()] first and then run all specifications using [specr()].
#'   This is the central function of the package. It runs the specification curve analysis. It takes the data frame and vectors for analytical choices related to the dependent variable, the independent variable, the type of models that should be estimated, the set of covariates that should be included (none, each individually, and all together), as well as a named list of potential subsets. The function returns a tidy tibble which includes relevant model parameters for each specification. The function \link[broom]{tidy} is used to extract relevant model parameters. Exactly what tidy considers to be a model component varies across models but is usually self-evident.
#'
#' @param df a data frame that includes all relevant variables
#' @param y a vector denoting the dependent variables
#' @param x a vector denoting independent variables
#' @param model a vector denoting the model(s) that should be estimated.
#' @param controls a vector denoting which control variables should be included. Defaults to NULL.
#' @param subsets a named list that includes potential subsets that should be evaluated (see examples). Defaults to NULL.
#' @param all.comb a logical value indicating what type of combinations of the control variables should be specified. Defaults to FALSE (i.e., none, all, and each individually). If this argument is set to TRUE, all possible combinations between the control variables are specified (see examples).
#' @param conf.level the confidence level to use for the confidence interval. Must be strictly greater than 0 and less than 1. Defaults to .95, which corresponds to a 95 percent confidence interval.
#' @param keep.results a logical value indicating whether the complete model object should be kept. Defaults to FALSE.
#'
#' @return a [tibble][tibble::tibble-package] that includes all specifications and a tidy summary of model components.
#'
#' @references \itemize{
#'  \item Simonsohn, U., Simmons, J. P., & Nelson, L. D. (2019). Specification Curve: Descriptive and Inferential Statistics for all Plausible Specifications. Available at: https://doi.org/10.2139/ssrn.2694998
#'  \item Steegen, S., Tuerlinckx, F., Gelman, A., & Vanpaemel, W. (2016). Increasing Transparency Through a Multiverse Analysis. Perspectives on Psychological Science, 11(5), 702-712. https://doi.org/10.1177/1745691616658637
#' }
#' @export
#'
#' @examples
#' # run specification curve analysis
#' results <- run_specs(df = example_data,
#'                      y = c("y1", "y2"),
#'                      x = c("x1", "x2"),
#'                      model = c("lm"),
#'                      controls = c("c1", "c2"),
#'                      subsets = list(group1 = unique(example_data$group1),
#'                                     group2 = unique(example_data$group2)))
#'
#' # Check results frame
#' results
#'
#' @seealso [plot_specs()] to visualize the results of the specification curve analysis.
run_specs <- function(df,
                      x,
                      y,
                      model = "lm",
                      controls = NULL,
                      subsets = NULL,
                      all.comb = FALSE,
                      conf.level = 0.95,
                      keep.results = FALSE) {

  # Deprecation warning
  lifecycle::deprecate_warn("1.0.0", "run_specs()", "specr()")

  if (rlang::is_missing(x)) {
    stop("You must specify at least one independent variable `x`.")
  }

  if (rlang::is_missing(y)) {
    stop("You must specify at least one dependent variable `y`.")
  }

  specs <- setup_specs(y = y,
                       x = x,
                       model = model,
                       controls = controls,
                       all.comb = all.comb)

  if (!is.null(subsets)) {

    if (!inherits(subsets, "list")) {
      wrong_class <- class(subsets)
      stop(glue("Subsets must be a 'list' and not a '{wrong_class}'."))
    }

  subsets <- map(subsets, as.character)

  df_list <- create_subsets(df, subsets)
  df_list[[length(df_list)+1]] <- df %>% dplyr::mutate(filter = "all")

  if (length(subsets) > 1) {

  suppressMessages({
  df_comb <- subsets %>%
    cross %>%
    map(~ create_subsets(subsets = .x, df = df) %>%
          map(~ dplyr::select(.x, -filter)) %>%
          reduce(dplyr::inner_join) %>%
          dplyr::mutate(filter = paste(names(.x),
                                       .x,
                                       collapse = " & ",
                                       sep = " = ")))

  df_all <- append(df_list, df_comb)
  })

  } else {

  df_all <- df_list

  }

  if (conf.level > 1 | conf.level < 0) {
    stop("The confidence level must be strictly greater than 0 and less than 1.")
  }

  res <- map_df(df_all, ~ run_spec(specs,
                            .x,
                            conf.level = conf.level,
                            keep.results = keep.results) %>%
                dplyr::mutate(subsets = unique(.x$filter)))

  } else {

  res <- run_spec(specs,
           df,
           conf.level = conf.level,
           keep.results = keep.results) %>%
    dplyr::mutate(subsets = "all")

  }

 return(res)

}




