
<!-- README.md is generated from README.Rmd. Please edit that file -->

# specr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/specr)](https://CRAN.R-project.org/package=specr)
<!-- badges: end -->

The goal of specr is to facilitate specification curve analyses as
described by Simonson, Simmons & Nelson (2015).\[1\] It provides
functions to setup, run, and plot all specifications.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("masurp/specr")
```

## Example

This is a basic example of how to use the major functions in this
package. In a first step, check the data (here a simulated data set).

``` r
# Load library
library(specr)
head(example_data)
#>         x1       x2        c1       c2       y1       y2 group1 group2
#> 1 1.942902 4.104683 0.5941764 3.700289 16.11449 16.04155      0      A
#> 2 2.512857 3.675712 4.1336440 2.378237 22.75160 22.79317      1      C
#> 3 3.053931 7.995717 2.0264256 5.254413 32.75778 36.06999      0      C
#> 4 2.279223 4.451733 3.3688941 3.220657 28.80391 21.77937      1      A
#> 5 3.599990 6.274502 3.6629448 2.955270 31.78923 32.05191      0      A
#> 6 2.762323 5.180676 1.5159513 3.132042 14.83215 21.36287      1      C
```

In a second step, we only need to use the function `run_specs()` and
include our analytical choices as arguments. The resulting data frame
includes relevant statistics of all models that were estimated.

``` r
# Self-made functions can be used too
mymodel <- function(formula, data) {
  glm(formula = formula, data = data, family = gaussian(link = "identity"))
}

# Run specification curve analysis
results <- run_specs(df = example_data, 
                     y = c("y1", "y2"), 
                     x = c("x1", "x2"), 
                     model = c("lm", "glm", "mymodel"), 
                     controls = c("c1", "c2"), 
                     subset = list(group1 = unique(example_data$group1)))
# Check results
head(results)
#> # A tibble: 6 x 9
#>   x     y     model controls estimate std.error statistic  p.value subset  
#>   <chr> <chr> <chr> <chr>       <dbl>     <dbl>     <dbl>    <dbl> <chr>   
#> 1 x1    y1    lm    c1 + c2      4.37    0.199       22.0 6.45e-60 group1 …
#> 2 x2    y1    lm    c1 + c2      1.98    0.0871      22.8 1.90e-62 group1 …
#> 3 x1    y2    lm    c1 + c2      4.25    0.176       24.1 1.08e-66 group1 …
#> 4 x2    y2    lm    c1 + c2      1.91    0.0784      24.4 1.15e-67 group1 …
#> 5 x1    y1    glm   c1 + c2      4.37    0.199       22.0 6.45e-60 group1 …
#> 6 x2    y1    glm   c1 + c2      1.98    0.0871      22.8 1.90e-62 group1 …
```

In a final step, we can use the function `plot_specs()` to produce a
typical visualization of the specification curve and how the analytical
choices affected the obtained results.

``` r
# Plot specification curve analysis
plot_specs(results)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

## References

1.  <http://sticerd.lse.ac.uk/seminarpapers/psyc16022016.pdf>
