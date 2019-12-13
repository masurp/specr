
<!-- README.md is generated from README.Rmd. Please edit that file -->

<div style="padding-top:1em; padding-bottom: 0.5em;">

<img src="man/figures/specr_logo.png" width = 135 align="right" />

</div>

# specr - Statistical Functions for Specification Curve Analyses

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/specr)](https://CRAN.R-project.org/package=specr)
<!-- badges: end -->

## Overview

The goal of specr is to facilitate specification curve analyses as
described by Simonson, Simmons & Nelson (2019). It can be used to
investigate how different (e.g., theoretical defensible) analytical
choices affect outcome statistics within the universe of one single data
set.

It provides functions to setup, run, evaluate, and plot the multiverse
of specifications. A simple usage example is provided below. For more
information about the various functions and specific use cases, visit
the documentation at <https://masurp.github.io/specr/index.html>.

## Disclaimer

We do see a lot of value in investigating how analytical choices affect
a statistical outcome of interest. However, we strongly caution against
using `specr` as a tool to somehow arrive at a better estimate. Running
a specification curve analysis does not make your findings any more
reliable, valid or generalizable than a single analyis. The method is
only meant to inform about the effects of analytical choices on results,
and not a better way to estimate a correlation or effect.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("masurp/specr")
```

## A Simple Usage Example

This is a basic example of how to use the major functions in this
package. In a first step, check the data (here a simulated data set that
is provided within the package).

``` r
library(specr)
head(example_data)
#>         x1         x2         c1        c2        y1         y2 group1
#> 1 1.533913  1.3697122  0.5424902 3.8924435 23.500543 10.4269278      0
#> 2 1.680639  1.5163745 -1.2415868 3.3377268 17.017955  0.5733467      1
#> 3 1.223941 -0.2381044  0.1405891 0.8911959 -3.678272  4.2303190      0
#> 4 1.765276  0.9524049  4.0397943 1.8567454 21.668684 14.8865252      1
#> 5 1.907134  0.6282816  3.1002518 5.5840574 32.713106 20.5251920      0
#> 6 1.710695  1.1898467  0.4648824 4.0239483 20.422171  4.3471236      1
#>   group2
#> 1      A
#> 2      C
#> 3      B
#> 4      B
#> 5      A
#> 6      C
```

In a second step, use the function `run_specs()` and include your
analytical choices as arguments. The resulting data frame includes
relevant statistics of all models that were estimated.

``` r
results <- run_specs(df = example_data, 
                     y = c("y1", "y2"), 
                     x = c("x1", "x2"), 
                     model = c("lm"), 
                     controls = c("c1", "c2"), 
                     subsets = list(group1 = unique(example_data$group1),
                                   group2 = unique(example_data$group2)))
head(results)
#> # A tibble: 6 x 12
#>   x     y     model controls estimate std.error statistic  p.value conf.low
#>   <chr> <chr> <chr> <chr>       <dbl>     <dbl>     <dbl>    <dbl>    <dbl>
#> 1 x1    y1    lm    c1 + c2     4.95      0.525     9.43  3.11e-18    3.92 
#> 2 x2    y1    lm    c1 + c2     6.83      0.321    21.3   1.20e-57    6.20 
#> 3 x1    y2    lm    c1 + c2    -0.227     0.373    -0.607 5.44e- 1   -0.961
#> 4 x2    y2    lm    c1 + c2     0.985     0.324     3.04  2.62e- 3    0.347
#> 5 x1    y1    lm    c1          5.53      0.794     6.97  2.95e-11    3.96 
#> 6 x2    y1    lm    c1          8.07      0.557    14.5   6.90e-35    6.98 
#> # … with 3 more variables: conf.high <dbl>, obs <int>, subsets <chr>
```

Use the function `summarise_specs()` to get a first summary of your
results (you can specify what type of statistics should be computed as
well as what grouping factors should be used).

``` r
summarise_specs(results, 
                stats = lst(median, min, max), 
                group = c("x", "y"))
#> # A tibble: 4 x 6
#> # Groups:   x [2]
#>   x     y     median    min   max   obs
#>   <chr> <chr>  <dbl>  <dbl> <dbl> <dbl>
#> 1 x1    y1     6.52   3.49   9.28   123
#> 2 x1    y2     0.498 -2.05   3.67   123
#> 3 x2    y1     7.80   5.89   9.58   123
#> 4 x2    y2     1.29  -0.258  2.91   123
```

Use the function `plot_specs()` to produce a typical visualization of
the specification curve and how the analytical choices affected the
obtained results.

``` r
# Plot specification curve analysis
plot_specs(results, choices = c("x", "y", "controls", "subsets"))
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

For a more comprehensive example, check out the page “Getting started”.
For specific uses and additional functions, have a look at the
vignettes.

## How to cite this package

Developing and maintaining open source software is an important yet
often underappreciated contribution to scientific progress. Thus,
whenever you are using open source software (or software in general),
please make sure to cite it appropriately so that developers get credit
for their work.

When using `specr`, please cite it as follows:

``` r
citation("specr")
#> 
#> To cite parameters in publications use:
#> 
#>   Masur, Philipp K. & Scharkow, M. (2019). specr: Statistical
#>   functions for conducting specification curve analyses. Available
#>   from https://github.com/masurp/specr.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Misc{,
#>     title = {specr: Statistical functions for conducting specification curve analyses (Version 0.1.0)},
#>     author = {Philipp K. Masur and Michael Scharkow},
#>     year = {2019},
#>     url = {https://github.com/masurp/specr},
#>   }
```

## References

Simonsohn, U., Simmons, J. P., & Nelson, L. D. (2019). Specification
Curve: Descriptive and Inferential Statistics for all Plausible
Specifications Available at:
<http://urisohn.com/sohn_files/wp/wordpress/wp-content/uploads/Paper-Specification-curve-2019-11-16.pdf>
