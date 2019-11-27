#' Get variance component of the specification curve
#'
#' @param df a data frame containing the choices and results of each specification (resulting from \code{run_specs}).
#' @param estimate the statistic to be evaluated (e.g., the unstandardized effect or the p-value)
#' @param percent a logical value indicating whether the icc should also be printed as percentage.
#'
#' @return
#' @export
#'
#' @examples
variance_specs <- function(df,
                           estimate,
                           percent = TRUE) {

  # dependencies
  require(lme4)

  # get random effect variances
  var <- lmer(estimate ~ 1 + (1|x)+ (1|y) + (1|model) + (1|controls) + (1|subset), data = results) %>%
    VarCorr %>%
    as.data.frame %>%
    select(grp, vcov)

  # sum all variance components
  sum_var <- sum(var$vcov)

  # estimate icc
  var <- var %>%
    mutate(icc = vcov/sum_var) %>%
    tbl_df

  if (isTRUE(percent)) {
    var <- var %>%
      mutate(percent = icc*100)
  }

  return(var)

}

