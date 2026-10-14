# Estimate all specifications

**\[deprecated\]** This function was deprecated because the new version
of specr uses different analytical framework. In this framework, you
should use the function
[`setup()`](https://masurp.github.io/specr/reference/setup.md) first and
then run all specifications using
[`specr()`](https://masurp.github.io/specr/reference/specr.md). This is
the central function of the package. It runs the specification curve
analysis. It takes the data frame and vectors for analytical choices
related to the dependent variable, the independent variable, the type of
models that should be estimated, the set of covariates that should be
included (none, each individually, and all together), as well as a named
list of potential subsets. The function returns a tidy tibble which
includes relevant model parameters for each specification. The function
[tidy](https://generics.r-lib.org/reference/tidy.html) is used to
extract relevant model parameters. Exactly what tidy considers to be a
model component varies across models but is usually self-evident.

## Usage

``` r
run_specs(
  df,
  x,
  y,
  model = "lm",
  controls = NULL,
  subsets = NULL,
  all.comb = FALSE,
  conf.level = 0.95,
  keep.results = FALSE
)
```

## Arguments

- df:

  a data frame that includes all relevant variables

- x:

  a vector denoting independent variables

- y:

  a vector denoting the dependent variables

- model:

  a vector denoting the model(s) that should be estimated.

- controls:

  a vector denoting which control variables should be included. Defaults
  to NULL.

- subsets:

  a named list that includes potential subsets that should be evaluated
  (see examples). Defaults to NULL.

- all.comb:

  a logical value indicating what type of combinations of the control
  variables should be specified. Defaults to FALSE (i.e., none, all, and
  each individually). If this argument is set to TRUE, all possible
  combinations between the control variables are specified (see
  examples).

- conf.level:

  the confidence level to use for the confidence interval. Must be
  strictly greater than 0 and less than 1. Defaults to .95, which
  corresponds to a 95 percent confidence interval.

- keep.results:

  a logical value indicating whether the complete model object should be
  kept. Defaults to FALSE.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
that includes all specifications and a tidy summary of model components.

## References

- Simonsohn, U., Simmons, J. P., & Nelson, L. D. (2019). Specification
  Curve: Descriptive and Inferential Statistics for all Plausible
  Specifications. Available at: https://doi.org/10.2139/ssrn.2694998

- Steegen, S., Tuerlinckx, F., Gelman, A., & Vanpaemel, W. (2016).
  Increasing Transparency Through a Multiverse Analysis. Perspectives on
  Psychological Science, 11(5), 702-712.
  https://doi.org/10.1177/1745691616658637

## See also

[`plot_specs()`](https://masurp.github.io/specr/reference/plot_specs.md)
to visualize the results of the specification curve analysis.

## Examples

``` r
# run specification curve analysis
results <- run_specs(df = example_data,
                     y = c("y1", "y2"),
                     x = c("x1", "x2"),
                     model = c("lm"),
                     controls = c("c1", "c2"),
                     subsets = list(group1 = unique(example_data$group1),
                                    group2 = unique(example_data$group2)))

# Check results frame
results
#> # A tibble: 192 × 23
#>    x     y     model controls estimate std.error statistic  p.value conf.low
#>    <chr> <chr> <chr> <chr>       <dbl>     <dbl>     <dbl>    <dbl>    <dbl>
#>  1 x1    y1    lm    c1 + c2    0.580     0.0698      8.30 2.69e-15   0.443 
#>  2 x2    y1    lm    c1 + c2    0.173     0.0559      3.10 2.12e- 3   0.0632
#>  3 x1    y2    lm    c1 + c2   -0.232     0.0698     -3.32 9.94e- 4  -0.369 
#>  4 x2    y2    lm    c1 + c2    0.0872    0.0522      1.67 9.58e- 2  -0.0155
#>  5 x1    y1    lm    c1         0.599     0.0679      8.81 7.26e-17   0.465 
#>  6 x2    y1    lm    c1         0.204     0.0538      3.79 1.78e- 4   0.0981
#>  7 x1    y2    lm    c1        -0.387     0.0765     -5.05 7.25e- 7  -0.537 
#>  8 x2    y2    lm    c1        -0.0693    0.0576     -1.20 2.30e- 1  -0.183 
#>  9 x1    y1    lm    c2         0.596     0.0678      8.79 8.65e-17   0.463 
#> 10 x2    y1    lm    c2         0.203     0.0525      3.87 1.29e- 4   0.100 
#> # ℹ 182 more rows
#> # ℹ 14 more variables: conf.high <dbl>, fit_r.squared <dbl>,
#> #   fit_adj.r.squared <dbl>, fit_sigma <dbl>, fit_statistic <dbl>,
#> #   fit_p.value <dbl>, fit_df <dbl>, fit_logLik <dbl>, fit_AIC <dbl>,
#> #   fit_BIC <dbl>, fit_deviance <dbl>, fit_df.residual <int>, fit_nobs <int>,
#> #   subsets <chr>
```
