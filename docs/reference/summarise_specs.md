# Summarise specifications

**\[deprecated\]** This function is deprecated because the new version
of specr uses a new analytic framework. In this framework, you can
obtain a similar summary simply by using the generic
[`summary()`](https://rdrr.io/r/base/summary.html) function and adding
the argument `type = "curve"`. This function allows to inspect results
of the specification curves by returning a comparatively simple summary
of the results. This summary can be produced for various specific
analytical choices and customized summary functions.

## Usage

``` r
summarise_specs(
  df,
  ...,
  var = .data$estimate,
  stats = list(median = median, mad = mad, min = min, max = max, q25 = function(x)
    quantile(x, prob = 0.25), q75 = function(x) quantile(x, prob = 0.75))
)
```

## Arguments

- df:

  a data frame resulting from
  [`run_specs()`](https://masurp.github.io/specr/reference/run_specs.md).

- ...:

  one or more grouping variables (e.g., subsets, controls,...) that
  denote the available analytical choices.

- var:

  which variable should be evaluated? Defaults to estimate (the effect
  sizes computed by
  [`run_specs()`](https://masurp.github.io/specr/reference/run_specs.md)).

- stats:

  named vector or named list of summary functions (individually defined
  summary functions can included). If it is not named, placeholders
  (e.g., "fn1") will be used as column names.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html).

## See also

[`plot_summary()`](https://masurp.github.io/specr/reference/plot_summary.md)
to visually investigate the affect of analytical choices.

## Examples

``` r
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
#> Warning: `summarise_specs()` was deprecated in specr 1.0.0.
#> ℹ Please use `summary(<specr.object>, type = "curve")` instead.
#> # A tibble: 1 × 7
#>   median   mad    min   max    q25   q75   obs
#>    <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl> <dbl>
#> 1  0.129 0.452 -0.490 0.678 -0.161 0.440   250

# Summary of specific analytical choices
summarise_specs(results,    # data frame
                x, y)       # analytical choices
#> # A tibble: 4 × 9
#>   x     y       median    mad    min    max     q25     q75   obs
#>   <chr> <chr>    <dbl>  <dbl>  <dbl>  <dbl>   <dbl>   <dbl> <dbl>
#> 1 x1    y1     0.605   0.0446  0.446  0.678  0.589   0.638    250
#> 2 x1    y2    -0.272   0.0905 -0.490 -0.124 -0.314  -0.207    250
#> 3 x2    y1     0.229   0.0651  0.107  0.438  0.193   0.276    250
#> 4 x2    y2    -0.00536 0.115  -0.204  0.184 -0.0784  0.0731   250

# Summary of other parameters across several analytical choices
summarise_specs(results,
                subsets, controls,
                var = p.value,
                stats = list(median = median,
                             min = min,
                             max = max))
#> # A tibble: 48 × 6
#>    subsets                           controls      median      min     max   obs
#>    <chr>                             <chr>          <dbl>    <dbl>   <dbl> <dbl>
#>  1 all                               c1          1.40e-14 4.13e-49 1.02e-3  1000
#>  2 all                               c1 + c2     2.32e- 8 2.08e-46 7.27e-2  1000
#>  3 all                               c2          4.76e- 8 7.41e-51 9.24e-2  1000
#>  4 all                               no covaria… 1.91e-14 8.09e-54 9.07e-4  1000
#>  5 group1 = middle                   c1          8.92e- 5 7.26e-17 2.30e-1   331
#>  6 group1 = middle                   c1 + c2     1.56e- 3 2.69e-15 9.58e-2   331
#>  7 group1 = middle                   c2          5.67e- 4 8.65e-17 1.52e-1   331
#>  8 group1 = middle                   no covaria… 5.05e- 6 2.35e-18 2.14e-1   331
#>  9 group1 = middle & group2 = female c1          7.73e- 2 3.24e- 8 3.48e-1   168
#> 10 group1 = middle & group2 = female c1 + c2     4.10e- 2 6.64e- 8 2.05e-1   168
#> # ℹ 38 more rows

# Named vector instead of named list passed to `stats`
summarise_specs(results,
                controls,
                stats = c(mean = mean,
                          median = median))
#> # A tibble: 4 × 4
#>   controls       mean median   obs
#>   <chr>         <dbl>  <dbl> <dbl>
#> 1 c1            0.101  0.101   250
#> 2 c1 + c2       0.166  0.122   250
#> 3 c2            0.177  0.138   250
#> 4 no covariates 0.115  0.102   250
```
