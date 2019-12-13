#' Get variance component of the specification curve
#'
#' @param model a multilevel model that captures the variances of the specification curve (resulting from \code{run_specs}).
#' @param percent a logical value indicating whether the icc should also be printed as percentage.
#'
#' @return
#' @export
#'
#' @examples
#' #+ setup, echo = F, message = F, warning = F
#' # Step 1: Run spec curve analysis
#' results <- run_specs(df = example_data,
#'                      y = c("y1", "y2"),
#'                      x = c("x1", "x2"),
#'                      model = c("lm"))
#'
#' # Step 2: Estimate multilevel model
#' library(lme4)
#' model <- lmer(estimate ~ 1 + (1|x)  + (1|y), data = results)
#'
#' # Step 3: Estimate intra-class correlation
#' icc_specs(model)
icc_specs <- function(model,
                      percent = TRUE) {

    # get variance components
    var <- model %>%
      VarCorr %>%
      as.data.frame %>%
      select(grp, vcov)

    # sum all variance components
    sum_var <- sum(var$vcov)

    # estimate icc
    var <- var %>%
      dplyr::mutate(icc = vcov/sum_var)

    if (isTRUE(percent)) {
      var <- var %>%
        dplyr::mutate(percent = icc*100)
    }

    return(var)

}
