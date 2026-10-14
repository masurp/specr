#' Compute intraclass correlation coefficient
#'
#' @description `r lifecycle::badge("deprecated")`
#'    This function is deprecated because the new version of specr uses a new analytic framework.
#'    In this framework, you can decompose the variance simply by using the generic \code{plot()}
#'    function and adding the argument \code{type = "variance"}, which also prints the variance components.
#'    This function extracts intraclass correlation coefficients (ICC) from a multilevel model. It can be used to decompose the variance in the outcome variable of a specification curve analysis (e.g., the regression coefficients). This approach summarises the relative importance of analytical choices by estimating the share of variance in the outcome (e.g., the regression coefficient) that different analytical choices or combinations therefor account for. To use this approach, one needs to estimate a multilevel model that includes all analytical choices as grouping variables (see examples).
#'
#' @param model a multilevel (i.e., mixed effects) model that captures the variances of the specification curve.
#' @param percent a logical value indicating whether the ICC should also be printed as percentage. Defaults to TRUE.
#'
#' @return a data frame including the grouping variable, the random effect variances, the raw intraclass correlation coefficient (ICC), and the ICC in percent.
#'
#' @references \itemize{
#'  \item Hox, J. J. (2010). Multilevel analysis: techniques and applications. New York: Routledge.
#' }
#' @export
#'
#' @examples
#' # Step 1: Run spec curve analysis
#' results <- run_specs(df = example_data,
#'                      y = c("y1", "y2"),
#'                      x = c("x1", "x2"),
#'                      model = c("lm"))
#'
#' # Step 2: Estimate a multilevel model without predictors
#' model <- lme4::lmer(estimate ~ 1 + (1|x)  + (1|y), data = results)
#'
#' # Step 3: Estimate intra-class correlation
#' icc_specs(model)
#'
#' @seealso [plot_variance()] to plot the variance decomposition.
icc_specs <- function(model,
                      percent = TRUE) {

  # Deprecation warning
  lifecycle::deprecate_warn("1.0.0", "icc_specs()", I('`plot(<specr.object>, type = "variance")`'))

  icc_specs_(model, percent = percent)
}

# Internal version without deprecation warning (also used by plot_variance())
icc_specs_ <- function(model,
                       percent = TRUE) {

    # get variance components (only variances, no covariances of random slopes)
    var <- model %>%
      VarCorr %>%
      as.data.frame %>%
      dplyr::filter(is.na(.data$var2)) %>%
      dplyr::select("grp", "vcov")

    # sum all variance components
    sum_var <- sum(var$vcov)

    # estimate icc
    var <- var %>%
      dplyr::mutate(icc = .data$vcov/sum_var)

    # include percentage
    if (isTRUE(percent)) {
      var <- var %>%
        dplyr::mutate(percent = .data$icc*100)
    }

    return(var)
}
