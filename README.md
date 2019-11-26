
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
d <- example_data
head(d)
#>          x1        x2          c1       c2 gender       y1       y2
#> 1 3.5835162 6.7585730 -0.08479593 1.046597      0 26.73193 28.45278
#> 2 1.5918187 3.9296317  6.37908937 2.974318      1 41.77180 42.09489
#> 3 0.4962778 0.5390142  3.03729593 3.941277      0 18.10251 18.67046
#> 4 0.5755038 2.1816832  5.26185866 2.083192      1 25.81769 25.09875
#> 5 1.7683538 3.1776625  1.62403890 1.054171      0 19.84352 18.74082
#> 6 2.3275039 5.6656614  4.34043420 2.218534      1 42.13992 42.14452
```

In a second step, we only need to use the function `run_specs()` and
include our analytical choices as arguments. The resulting data frame
includes relevant statistics of all models that were estimated.

``` r
# Run specification curve analysis
results <- run_specs(df = d, 
                     y = c("y1", "y2"), 
                     x = c("x1", "x2"), 
                     model = c("lm", "glm"), 
                     controls = c("c1", "c2"), 
                     subset = list(gender = unique(d$gender)))
# Check results
head(results)
#> # A tibble: 6 x 9
#>   x     y     model controls estimate std.error statistic   p.value subset 
#>   <chr> <chr> <chr> <chr>       <dbl>     <dbl>     <dbl>     <dbl> <chr>  
#> 1 x1    y1    lm    c1 + c2      8.93    0.232       38.5 3.14e-106 gender…
#> 2 x2    y1    lm    c1 + c2      4.32    0.0716      60.3 2.40e-149 gender…
#> 3 x1    y2    lm    c1 + c2      9.03    0.242       37.3 2.67e-103 gender…
#> 4 x2    y2    lm    c1 + c2      4.40    0.0694      63.3 2.67e-154 gender…
#> 5 x1    y1    glm   c1 + c2      8.93    0.232       38.5 3.14e-106 gender…
#> 6 x2    y1    glm   c1 + c2      4.32    0.0716      60.3 2.40e-149 gender…
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
