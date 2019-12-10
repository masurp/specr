#' Plot variance components of the specification curve
#'
#' @param model a multilevel model that captures the variances of the specification curve (resulting from \code{run_specs}).
#'
#' @return
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
#' library(lme4)
#' model <- lmer(estimate ~ 1 + (1|x)  + (1|y), data = results)
#'
#' # Step 3: Plot model
#' plot_variance(model)
plot_variance <- function(model) {

  # dependencies
  require(ggplot2)

  icc_specs(model) %>%
    ggplot(aes(x = grp,
               y = percent)) +
    geom_bar(stat = "identity", fill = "#377eb8") +
    theme_minimal() +
    theme(axis.text = element_text(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          axis.line.x = element_line(colour = "black")) +
    labs(x = "", y = "proportion of variance", fill = "analytical choices")
}
