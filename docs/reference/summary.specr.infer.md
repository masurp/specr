# Summarizing inference on the specification curve

Computes the three test statistics proposed by Simonsohn et al. (2020)
for the observed specification curve and for each curve under the null
(resulting from
[`infer_curve()`](https://masurp.github.io/specr/reference/infer_curve.md)),
and tests how extreme the observed statistics are compared to their
distribution under the null.

## Usage

``` r
# S3 method for class 'specr.infer'
summary(
  object,
  direction = c("two.sided", "greater", "less"),
  alpha = 0.05,
  group = NULL,
  ...
)
```

## Arguments

- object:

  A `specr.infer` object, resulting from calling
  [`infer_curve()`](https://masurp.github.io/specr/reference/infer_curve.md).

- direction:

  Direction of the alternative hypothesis. The default `"two.sided"`
  tests for an effect in either direction. Use `"greater"` or `"less"`
  only if the direction of the effect was predicted before the results
  were known.

- alpha:

  Significance level that is used to count significant specifications.
  Defaults to .05.

- group:

  Optional character vector with names of analytical choices (e.g.,
  `"y"` or `"subsets"`) to compute the test statistics separately within
  each group of specifications.

- ...:

  Further arguments passed to or from other methods (currently ignored).

## Value

A tibble with one row per test statistic (and group), including the
observed `estimate`, the 2.5% and 97.5% quantiles of the statistic under
the null (`null_lower` and `null_upper`), the `p.value`, and the number
of specifications (`n_specs`).

## Details

The three test statistics are:

- median:

  The median estimate across all specifications.

- share significant:

  The number of specifications with a significant estimate (p \<
  `alpha`) in the tested direction: positive for `"greater"`, negative
  for `"less"`, and the more frequent (dominant) direction for
  `"two.sided"`.

- aggregated z:

  Stouffer's Z: each p-value is converted into a z-value with the sign
  of its estimate, and the sum of these z-values is divided by the
  square root of the number of specifications.

The p-value of each statistic is the share of curves under the null
whose statistic is at least as extreme as the observed one, computed as
(1 + number of curves at least as extreme) / (1 + number of bootstrap
samples), so that it is never zero. For `"two.sided"`, "at least as
extreme" refers to the absolute value of the median and of Stouffer's Z,
and to the number of significant specifications in the dominant
direction of each curve. If the extraction function did not return
p-values, only the median can be tested.

## References

- Simonsohn, U., Simmons, J.P. & Nelson, L.D. (2020). Specification
  curve analysis. *Nature Human Behaviour, 4*, 1208–1214.
  https://doi.org/10.1038/s41562-020-0912-z

## See also

[`infer_curve()`](https://masurp.github.io/specr/reference/infer_curve.md),
[`plot.specr.infer()`](https://masurp.github.io/specr/reference/plot.specr.infer.md)

## Examples

``` r
specs <- setup(data = example_data,
   y = "y1",
   x = c("x1", "x2"),
   model = "lm",
   controls = c("c1", "c2"))

results <- specr(specs)

set.seed(42)
inference <- infer_curve(results, specs, n_samples = 200) # better 1,000 or more

# Two-sided tests (default)
summary(inference)
#> # A tibble: 3 × 6
#>   statistic                    estimate null_lower null_upper p.value n_specs
#>   <chr>                           <dbl>      <dbl>      <dbl>   <dbl>   <int>
#> 1 median                          0.432    -0.0591     0.0616 0.00498       8
#> 2 share significant (positive)    8         0          4.05   0.0149        8
#> 3 aggregated z                   32.8      -4.77       4.91   0.00498       8

# One-sided tests for a predicted positive effect
summary(inference, direction = "greater")
#> # A tibble: 3 × 6
#>   statistic                    estimate null_lower null_upper p.value n_specs
#>   <chr>                           <dbl>      <dbl>      <dbl>   <dbl>   <int>
#> 1 median                          0.432    -0.0591     0.0616 0.00498       8
#> 2 share significant (positive)    8         0          3.03   0.0149        8
#> 3 aggregated z                   32.8      -4.77       4.91   0.00498       8

# Separately for each independent variable
summary(inference, group = "x")
#> # A tibble: 6 × 7
#>   x     statistic                 estimate null_lower null_upper p.value n_specs
#>   <chr> <chr>                        <dbl>      <dbl>      <dbl>   <dbl>   <int>
#> 1 x1    median                       0.607    -0.0786     0.0861 0.00498       4
#> 2 x1    share significant (posit…    4         0          4      0.0398        4
#> 3 x1    aggregated z                29.7      -4.19       4.28   0.00498       4
#> 4 x2    median                       0.254    -0.0523     0.0602 0.00498       4
#> 5 x2    share significant (posit…    4         0          3      0.0199        4
#> 6 x2    aggregated z                16.6      -3.60       3.97   0.00498       4
```
