# Set up specifications

**\[deprecated\]** This function was deprecated because the new version
of specr uses a new analytic framework. In this framework, you should
use the function
[`setup()`](https://masurp.github.io/specr/reference/setup.md) instead.
This function creates a tibble that includes all possible specifications
based the dependent and independent variables, model types, and control
variables that are specified. This function simply produces a tibble of
all combinations. It can be used to check the specified analytical
choices. This function is called within
[`run_specs()`](https://masurp.github.io/specr/reference/run_specs.md),
which estimates all specified models based on the data that are
provided.

## Usage

``` r
setup_specs(x, y, model, controls = NULL, all.comb = FALSE)
```

## Arguments

- x:

  a vector denoting independent variables

- y:

  a vector denoting the dependent variables

- model:

  a vector denoting the model(s) that should be estimated.

- controls:

  a vector of the control variables that should be included. Defaults to
  NULL.

- all.comb:

  a logical value indicating what type of combinations of the control
  variables should be specified. Defaults to FALSE (i.e., none, all, and
  each individually). If this argument is set to TRUE, all possible
  combinations between the control variables are specified (see
  examples).

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
that includes all possible specifications based on combinations of the
analytic choices.

## See also

[`run_specs()`](https://masurp.github.io/specr/reference/run_specs.md)
to run the specification curve analysis.

## Examples

``` r
setup_specs(x = c("x1", "x2"),
            y = "y2",
            model = "lm",
            controls = c("c1", "c2"))
#> Warning: `setup_specs()` was deprecated in specr 1.0.0.
#> ℹ Please use `setup()` instead.
#> # A tibble: 8 × 4
#>   x     y     model controls     
#>   <chr> <chr> <chr> <chr>        
#> 1 x1    y2    lm    c1 + c2      
#> 2 x2    y2    lm    c1 + c2      
#> 3 x1    y2    lm    c1           
#> 4 x2    y2    lm    c1           
#> 5 x1    y2    lm    c2           
#> 6 x2    y2    lm    c2           
#> 7 x1    y2    lm    no covariates
#> 8 x2    y2    lm    no covariates
```
