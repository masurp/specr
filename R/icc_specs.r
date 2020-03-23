#' Get variance component of the specification curve
#'
#' Can be used to extract intraclass correlation coefficients from a multilevel model.
#'
#' @param model a multilevel model that captures the variances of the specification curve (resulting from \code{run_specs}).
#' @param percent a logical value indicating whether the icc should also be printed as percentage.
#'
#' @return a [tibble][tibble::tibble-package]
#'
#' @references \itemize{
#'  \item Hox, J. J. (2010). Multilevel analysis: techniques and applications (2nd ed). New York: Routledge.
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
#' # Step 2: Estimate multilevel model
#' model <- lme4::lmer(estimate ~ 1 + (1|x)  + (1|y), data = results)
#'
#' # Step 3: Estimate intra-class correlation
#' icc_specs(model)
icc_specs <- function(model,
                      percent = TRUE) {

    # get variance components
    var <- model %>%
      VarCorr %>%
      as.data.frame %>%
      dplyr::select(.data$grp, .data$vcov)

    # sum all variance components
    sum_var <- sum(var$vcov)

    # estimate icc
    var <- var %>%
      dplyr::mutate(icc = vcov/sum_var)

    # include percentage
    if (isTRUE(percent)) {
      var <- var %>%
        dplyr::mutate(percent = .data$icc*100)
    }

    return(var)

}
