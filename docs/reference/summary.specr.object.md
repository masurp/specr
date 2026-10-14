# Summarizing the Specification Curve Analysis

`summary` method for class "specr.object". With `type = "default"`, it
provides a printed output including technical details (e.g., cores used,
duration of the fitting process, number of specifications), a
descriptive analysis of the overall specification curve, a descriptive
summary of the resulting sample sizes, and a head of the results. With
`type = "curve"`, it returns descriptive statistics of the specification
curve (overall or for groups of specifications).

## Usage

``` r
# S3 method for class 'specr.object'
summary(
  object,
  type = "default",
  group = NULL,
  var = .data$estimate,
  stats = list(median = median, mad = mad, min = min, max = max, q25 = function(x)
    quantile(x, prob = 0.25), q75 = function(x) quantile(x, prob = 0.75)),
  digits = 2,
  rows = 6,
  ...
)
```

## Arguments

- object:

  An object of class "specr.object", usually resulting of a call to
  `specr`.

- type:

  What should be summarized? Either `"default"` (printed summary of the
  whole analysis) or `"curve"` (tibble with descriptive statistics of
  the curve).

- group:

  In combination with `type = "curve"`, a character vector of one or
  more variables (e.g., "subsets", "controls",...) that denote the
  available analytic choices to group the summary of the estimate.

- var:

  Unquoted name of the parameter to be summarized. Defaults to estimate.

- stats:

  Named vector or named list of summary functions (individually defined
  summary functions can included). If it is not named, placeholders
  (e.g., "fn1") will be used as column names.

- digits:

  The number of digits to use when printing the specification table.

- rows:

  The number of rows of the specification tibble that should be printed.

- ...:

  further arguments passed to or from other methods (currently ignored).

## Value

For `type = "default"`, a printed summary of an object of class
`specr.object`, which is returned invisibly. For `type = "curve"`, a
tibble with the descriptive statistics (and the median sample size
`obs`, if available).

## See also

The function used to create the "specr.object" object:
[`specr()`](https://masurp.github.io/specr/reference/specr.md).

## Examples

``` r
# Setup up specifications (returns object of class "specr.setup")
specs <- setup(data = example_data,
   y = c("y1", "y2"),
   x = c("x1", "x2"),
   model = "lm",
   controls = c("c1", "c2"),
   subsets = list(group1 = unique(example_data$group1)))

# Run analysis (returns object of class "specr.object")
results <- specr(specs)

# Default summary of the "specr.object"
summary(results)
#> Results of the specification curve analysis
#> -------------------
#> Technical details:
#> 
#>   Class:                          specr.object -- version: 1.1.0 
#>   Cores used:                     1 
#>   Duration of fitting process:    0.119 sec elapsed 
#>   Number of specifications:       64 
#> 
#> Descriptive summary of the specification curve:
#> 
#>  median  mad   min  max   q25  q75
#>    0.13 0.41 -0.39 0.66 -0.14 0.39
#> 
#> Descriptive summary of sample sizes: 
#> 
#>  median min  max
#>   338.5 323 1000
#> 
#> Head of the specification results (first 6 rows): 
#> 
#> # A tibble: 6 × 25
#>   x     y     model controls subsets group1 formula estimate std.error statistic
#>   <chr> <chr> <chr> <chr>    <chr>   <fct>  <glue>     <dbl>     <dbl>     <dbl>
#> 1 x1    y1    lm    no cova… middle  middle y1 ~ x…     0.61      0.07      9.28
#> 2 x1    y1    lm    no cova… old     old    y1 ~ x…     0.66      0.06     10.4 
#> 3 x1    y1    lm    no cova… young   young  y1 ~ x…     0.57      0.06     10.2 
#> 4 x1    y1    lm    no cova… all     NA     y1 ~ x…     0.62      0.04     16.4 
#> 5 x1    y1    lm    c1       middle  middle y1 ~ x…     0.6       0.07      8.81
#> 6 x1    y1    lm    c1       old     old    y1 ~ x…     0.64      0.06      9.81
#> # ℹ 15 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>,
#> #   fit_r.squared <dbl>, fit_adj.r.squared <dbl>, fit_sigma <dbl>,
#> #   fit_statistic <dbl>, fit_p.value <dbl>, fit_df <dbl>, fit_logLik <dbl>,
#> #   fit_AIC <dbl>, fit_BIC <dbl>, fit_deviance <dbl>, fit_df.residual <dbl>,
#> #   fit_nobs <dbl>

# Summarize the specification curve descriptively
summary(results, type = "curve")
#> # A tibble: 1 × 7
#>   median   mad    min   max    q25   q75   obs
#>    <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1  0.126 0.412 -0.387 0.662 -0.139 0.395  338.

# Grouping for certain analytical decisions
summary(results,
       type = "curve",
       group = c("x", "y"))
#> # A tibble: 4 × 9
#>   x     y      median    mad    min     max     q25     q75   obs
#>   <chr> <chr>   <dbl>  <dbl>  <dbl>   <dbl>   <dbl>   <dbl> <dbl>
#> 1 x1    y1     0.601  0.0388  0.541  0.662   0.577   0.623   338.
#> 2 x1    y2    -0.258  0.0784 -0.387 -0.178  -0.314  -0.211   338.
#> 3 x2    y1     0.232  0.0573  0.159  0.346   0.200   0.283   338.
#> 4 x2    y2    -0.0211 0.115  -0.127  0.0917 -0.0882  0.0572  338.

# Using customized functions
summary(results,
        type = "curve",
       group = c("x", "group1"),
       stats = list(median = median,
                    min = min,
                    max = max))
#> # A tibble: 8 × 6
#>   x     group1 median     min   max   obs
#>   <chr> <fct>   <dbl>   <dbl> <dbl> <dbl>
#> 1 x1    middle  0.178 -0.387  0.613   331
#> 2 x1    old     0.221 -0.304  0.662   323
#> 3 x1    young   0.182 -0.310  0.567   346
#> 4 x1    NA      0.195 -0.339  0.620  1000
#> 5 x2    middle  0.130 -0.0693 0.228   331
#> 6 x2    old     0.204 -0.0836 0.346   323
#> 7 x2    young   0.101 -0.127  0.202   346
#> 8 x2    NA      0.144 -0.107  0.272  1000
```
