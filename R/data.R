#' Example data set
#'
#' This simulated data set can be used to explore the major function of 'specr'.
#'    It provides variables that can be used to mimic different independent and dependent variables,
#'    control variables, and grouping variables (for subset analyses).
#'
#' @docType data
#'
#' @usage data(example_data)
#'
#' @format A tibble with 1,000 rows and 15 variables (no missing values):
#' \describe{
#'   \item{group1}{Grouping variable with three groups ("young", "middle", "old"), e.g. for subset analyses.}
#'   \item{group2}{Grouping variable with two groups ("female", "male").}
#'   \item{group3}{Grouping variable with three groups ("A", "B", "C").}
#'   \item{x1, x2, x3, x4}{Numeric variables that can be used as (alternative) independent variables.}
#'   \item{c1, c2, c3, c4}{Numeric variables that can be used as control variables.}
#'   \item{y1, y2, y3, y4}{Numeric variables that can be used as (alternative) dependent variables.}
#' }
#'
#' @source Simulated data.
#'
#' @keywords datasets
#'
#' @examples
#' data(example_data)
#' head(example_data)
"example_data"
