
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
package

``` r
# Simulating some data
x1 <- rnorm(500, 2.5, 1)
x2 <- 2*x1 + rnorm(500, 0, 1)
c1 <- rnorm(500, 2.5, 2)
c2 <- rnorm(500, 2.5, 2)
gender <- rep(c(0,1), 250)
y1 <- 2*x1 + 2.5*x2 + 2*c1 + 2*c2 + 0.5*(x1*c1) + 0.25*(x2*c2) + 2*gender + rnorm(500, 0, 1)
y2 <- 2*x1 + 2.5*x2 + 2*c1 + 2*c2 + 0.5*(x1*c1) + 0.25*(x2*c2) + 2*gender + rnorm(500, 0, 1)
d <- data.frame(x1, x2, c1, c2, gender, y1, y2)

# Check data
head(d)
#>         x1       x2       c1       c2 gender       y1       y2
#> 1 1.923342 2.335086 1.102197 2.499008      0 19.84669 20.05541
#> 2 1.549052 2.951681 3.724179 3.537191      1 31.67117 32.24054
#> 3 1.639111 2.103567 3.480188 5.813824      0 31.88726 32.79715
#> 4 1.376519 3.746129 3.784648 3.038297      1 34.24784 32.76637
#> 5 2.887784 5.682990 3.101651 6.182157      0 51.41092 51.60973
#> 6 1.502650 4.247065 4.188093 1.302151      1 31.58377 31.92277

# Load library
library(specr)

# Run specification curve analysis
results <- run_specs(df = d, 
                  y = c("y1", "y2"), 
                  x = c("x1", "x2"), 
                  model = "glm", 
                  controls = c("c1", "c2"), 
                  subset = list(gender = unique(d$gender)))
# Check results
head(results)
#> # A tibble: 6 x 9
#>   x     y     model controls estimate std.error statistic   p.value subset 
#>   <chr> <chr> <chr> <chr>       <dbl>     <dbl>     <dbl>     <dbl> <chr>  
#> 1 x1    y1    glm   c1 + c2      9.60    0.242       39.7 6.55e-109 gender…
#> 2 x2    y1    glm   c1 + c2      4.54    0.0686      66.2 7.73e-159 gender…
#> 3 x1    y2    glm   c1 + c2      9.69    0.233       41.6 2.37e-113 gender…
#> 4 x2    y2    glm   c1 + c2      4.56    0.0677      67.3 1.66e-160 gender…
#> 5 x1    y1    glm   c1           9.98    0.508       19.6 2.36e- 52 gender…
#> 6 x2    y1    glm   c1           4.76    0.208       22.9 4.12e- 63 gender…

# Plot specification curve analysis
plot_specs(results)
```

<img src="man/figures/README-example-1.png" width="100%" />

1.  <http://sticerd.lse.ac.uk/seminarpapers/psyc16022016.pdf>
