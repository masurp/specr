
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

This is a basic example which shows you how to solve a common problem:

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

# Load library
library(specr)

# Run specification curve analysis
results <- run_specs(df = d, 
                  y = c("y1", "y2"), 
                  x = c("x1", "x2"), 
                  model = "lm", 
                  controls = c("c1", "c2"), 
                  subset = list(gender = unique(d$gender)))
# Check results
head(results)

# Plot specification curve analysis
plot_specs(results)
```

1.  <http://sticerd.lse.ac.uk/seminarpapers/psyc16022016.pdf>
