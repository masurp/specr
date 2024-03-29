% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise_specs.r
\name{summarise_specs}
\alias{summarise_specs}
\title{Summarise specifications}
\usage{
summarise_specs(
  df,
  ...,
  var = .data$estimate,
  stats = list(median = median, mad = mad, min = min, max = max, q25 = function(x)
    quantile(x, prob = 0.25), q75 = function(x) quantile(x, prob = 0.75))
)
}
\arguments{
\item{df}{a data frame resulting from \code{run_specs()}.}

\item{...}{one or more grouping variables (e.g., subsets, controls,...) that denote the available analytical choices.}

\item{var}{which variable should be evaluated? Defaults to estimate (the effect sizes computed by \code{\link[=run_specs]{run_specs()}}).}

\item{stats}{named vector or named list of summary functions (individually defined summary functions can included). If it is not named, placeholders (e.g., "fn1") will be used as column names.}
}
\value{
a \link[tibble:tibble-package]{tibble}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
This function is deprecated because the new version of specr uses a new analytic framework.
In this framework, you can plot a similar figure simply by using the generic \code{plot()} function.
This function allows to inspect results of the specification curves by returning a comparatively simple summary of the results. This summary can be produced for various specific analytical choices and customized summary functions.
}
\examples{
# Run specification curve analysis
results <- run_specs(df = example_data,
                     y = c("y1", "y2"),
                     x = c("x1", "x2"),
                     model = c("lm"),
                     controls = c("c1", "c2"),
                     subsets = list(group1 = unique(example_data$group1),
                                    group2 = unique(example_data$group2)))

# overall summary
summarise_specs(results)

# Summary of specific analytical choices
summarise_specs(results,    # data frame
                x, y)       # analytical choices

# Summary of other parameters across several analytical choices
summarise_specs(results,
                subsets, controls,
                var = p.value,
                stats = list(median = median,
                             min = min,
                             max = max))

# Unnamed vector instead of named list passed to `stats`
summarise_specs(results,
                controls,
                stats = c(mean = mean,
                          median = median))

}
\seealso{
\code{\link[=plot_summary]{plot_summary()}} to visually investigate the affect of analytical choices.
}
