# Fit models across all specifications

Runs the specification/multiverse analysis across specified models. This
is the central function of the package and represent the second step in
the analytic framework implemented in the package `specr`. It estimates
and returns respective parameters and estimates of models that were
specified via
[`setup()`](https://masurp.github.io/specr/reference/setup.md).

## Usage

``` r
specr(x, data = NULL, ...)
```

## Arguments

- x:

  A `specr.setup` object resulting from `setup` or a tibble that
  contains the relevant specifications (e.g., a tibble resulting from
  `as_tibble(setup(...))`).

- data:

  If x is not an object of "specr.setup" and simply a tibble, you need
  to provide the data set that should be used. Defaults to NULL as it is
  assumed that most users will create an object of class "specr.setup"
  that they'll pass to `specr()`.

- ...:

  Further arguments that can be passed to `future_pmap`. This only
  becomes important if parallelization is used (they are ignored if the
  specifications are fitted sequentially). For example, `furrr_options`
  can be passed to the argument `.options`. When a plan for
  parallelization is set, one can also set `.progress = TRUE` to print a
  progress bar during the fitting process. See details for more
  information on parallelization.

## Value

An object of class `specr.object`, which includes a data frame with all
specifications their respective results along with many other useful
information about the model. Parameters are extracted via the function
passed to `setup`. By default this is
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
the function
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html).
Several other aspects and information are included in the resulting
class (e.g., number of specifications, time elapsed, subsets included in
the analyses). Use `methods(class = "specr.object")` for an overview on
available methods.

## Details

Empirical results are often contingent on analytical decisions that are
equally defensible, often arbitrary, and motivated by different reasons.
These decisions may introduce bias or at least variability. To this end,
specification curve analyses (Simonsohn et al., 2020) or multiverse
analyses (Steegen et al., 2016) refer to identifying the set of
theoretically justified, statistically valid (and potentially also
non-redundant) specifications, fitting the "multiverse" of models
represented by these specifications and extract relevant parameters
often to display the results graphically as a so-called specification
curve. This allows readers to identify consequential specifications
decisions and how they affect the results or parameter of interest.

**Use of this function**

A general overview is provided in the vignette
[`vignette("getting-started")`](https://masurp.github.io/specr/articles/getting-started.md).
Generally, you create relevant specification using the function
[`setup()`](https://masurp.github.io/specr/reference/setup.md). You then
pass the resulting object of a class `specr.setup` to the present
function `specr()` to run the specification curve analysis. Further note
that the resulting object of class `specr.object` allows to use several
generic function such as
[`summary()`](https://rdrr.io/r/base/summary.html) or
[`plot()`](https://rdrr.io/r/graphics/plot.default.html). Use
`methods(class = "specr.object")` for an overview on available methods
and e.g.,
[`?plot.specr.object`](https://masurp.github.io/specr/reference/plot.specr.object.md)
to view the dedicated help page.

**Parallelization**

By default, the function fits models across all specifications
sequentially (one after the other). If the data set is large, the models
complex (e.g., large structural equation models, negative binomial
models, or Bayesian models), and the number of specifications is large,
it can make sense to parallelize these operations. One simply has to
load the package `furrr` (which in turn, builds on `future`) up front.
Then parallelizing the fitting process works as specified in the package
description of `furrr`/`future` by setting a "plan" before running
`specr` such as:

`plan(multisession, workers = 4)`

However, there are many more ways to specifically set up the plan,
including different strategy than `multisession`. For more information,
see
[`vignette("parallelization")`](https://masurp.github.io/specr/articles/parallelization.md)
and the [reference
page](https://future.futureverse.org/reference/plan.html) for `plan()`.

**Disclaimer**

We do see a lot of value in investigating how analytical choices affect
a statistical outcome of interest. However, we strongly caution against
using specr as a tool to somehow arrive at a better estimate compared to
a single model. Running a specification curve analysis does not make
your findings any more reliable, valid or generalizable than a single
analysis. The method is meant to inform about the effects of analytical
choices on results, and not a better way to estimate a correlation or
effect.

## References

- Simonsohn, U., Simmons, J.P. & Nelson, L.D. (2020). Specification
  curve analysis. *Nature Human Behaviour, 4*, 1208–1214.
  https://doi.org/10.1038/s41562-020-0912-z

- Steegen, S., Tuerlinckx, F., Gelman, A., & Vanpaemel, W. (2016).
  Increasing Transparency Through a Multiverse Analysis. *Perspectives
  on Psychological Science, 11*(5), 702-712.
  https://doi.org/10.1177/1745691616658637

## See also

[`setup()`](https://masurp.github.io/specr/reference/setup.md) for the
first step of setting up the specifications.

[`summary.specr.object()`](https://masurp.github.io/specr/reference/summary.specr.object.md)
for how to summarize and inspect the results.

[`plot.specr.object()`](https://masurp.github.io/specr/reference/plot.specr.object.md)
for plotting results.

## Examples

``` r
# Example 1 ----
# Setup up typical specifications
specs <- setup(data = example_data,
   y = c("y1", "y2"),
   x = c("x1", "x2"),
   model = "lm",
   controls = c("c1", "c2"),
   subsets = list(group1 = unique(example_data$group1)))

# Run analysis (not parallelized)
results <- specr(specs)

# Summary of the results
summary(results)
#> Results of the specification curve analysis
#> -------------------
#> Technical details:
#> 
#>   Class:                          specr.object -- version: 1.1.0 
#>   Cores used:                     1 
#>   Duration of fitting process:    0.118 sec elapsed 
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


# Example 2 ----
# Working without S3 classes
specs2 <- setup(data = example_data,
    y = c("y1", "y2"),
    x = c("x1", "x2"),
    model = "lm",
    controls = "c1")

# Working with tibbles
specs_tibble <- as_tibble(specs2)      # extract tibble from setup
results2 <- specr(specs_tibble,
                  data = example_data) # need to provide data!

# Results (tibble instead of S3 class)
head(results2)
#> # A tibble: 6 × 26
#>   x     y     model controls      subsets formula  model_function term  estimate
#>   <chr> <chr> <chr> <chr>         <chr>   <glue>   <list>         <chr>    <dbl>
#> 1 x1    y1    lm    no covariates all     y1 ~ x1… <fn>           x1       0.620
#> 2 x1    y1    lm    c1            all     y1 ~ x1… <fn>           x1       0.603
#> 3 x1    y2    lm    no covariates all     y2 ~ x1… <fn>           x1      -0.328
#> 4 x1    y2    lm    c1            all     y2 ~ x1… <fn>           x1      -0.339
#> 5 x2    y1    lm    no covariates all     y1 ~ x2… <fn>           x2       0.272
#> 6 x2    y1    lm    c1            all     y1 ~ x2… <fn>           x2       0.249
#> # ℹ 17 more variables: std.error <dbl>, statistic <dbl>, p.value <dbl>,
#> #   conf.low <dbl>, conf.high <dbl>, fit_r.squared <dbl>,
#> #   fit_adj.r.squared <dbl>, fit_sigma <dbl>, fit_statistic <dbl>,
#> #   fit_p.value <dbl>, fit_df <dbl>, fit_logLik <dbl>, fit_AIC <dbl>,
#> #   fit_BIC <dbl>, fit_deviance <dbl>, fit_df.residual <int>, fit_nobs <int>
```
