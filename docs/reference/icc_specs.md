# Compute intraclass correlation coefficient

**\[deprecated\]** This function is deprecated because the new version
of specr uses a new analytic framework. In this framework, you can
decompose the variance simply by using the generic
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) function and
adding the argument `type = "variance"`, which also prints the variance
components. This function extracts intraclass correlation coefficients
(ICC) from a multilevel model. It can be used to decompose the variance
in the outcome variable of a specification curve analysis (e.g., the
regression coefficients). This approach summarises the relative
importance of analytical choices by estimating the share of variance in
the outcome (e.g., the regression coefficient) that different analytical
choices or combinations therefor account for. To use this approach, one
needs to estimate a multilevel model that includes all analytical
choices as grouping variables (see examples).

## Usage

``` r
icc_specs(model, percent = TRUE)
```

## Arguments

- model:

  a multilevel (i.e., mixed effects) model that captures the variances
  of the specification curve.

- percent:

  a logical value indicating whether the ICC should also be printed as
  percentage. Defaults to TRUE.

## Value

a data frame including the grouping variable, the random effect
variances, the raw intraclass correlation coefficient (ICC), and the ICC
in percent.

## References

- Hox, J. J. (2010). Multilevel analysis: techniques and applications.
  New York: Routledge.

## See also

[`plot_variance()`](https://masurp.github.io/specr/reference/plot_variance.md)
to plot the variance decomposition.

## Examples

``` r
# Step 1: Run spec curve analysis
results <- run_specs(df = example_data,
                     y = c("y1", "y2"),
                     x = c("x1", "x2"),
                     model = c("lm"))
#> Warning: `run_specs()` was deprecated in specr 1.0.0.
#> ℹ Please use `specr()` instead.
#> ℹ Set up the specifications with `setup()` first.

# Step 2: Estimate a multilevel model without predictors
model <- lme4::lmer(estimate ~ 1 + (1|x)  + (1|y), data = results)

# Step 3: Estimate intra-class correlation
icc_specs(model)
#> Warning: `icc_specs()` was deprecated in specr 1.0.0.
#> ℹ Please use `plot(<specr.object>, type = "variance")` instead.
#>        grp         vcov          icc      percent
#> 1        x 2.078321e-09 8.647959e-09 8.647959e-07
#> 2        y 1.971133e-01 8.201945e-01 8.201945e+01
#> 3 Residual 4.321178e-02 1.798055e-01 1.798055e+01
```
