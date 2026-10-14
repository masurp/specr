# Summarizing the Specifications Setup

`summary` method for class "specr.setup". Provides a short summary of
the created specifications (the "multiverse") that lists all analytic
choices, prints the function used to extract the parameters from the
model. Finally, if `print.specs = TRUE`, it also shows the head of the
actual specification grid.

## Usage

``` r
# S3 method for class 'specr.setup'
summary(object, digits = 2, rows = 6, print.specs = TRUE, ...)
```

## Arguments

- object:

  An object of class "specr.setup", usually, a result of a call to
  `setup`.

- digits:

  The number of digits to use when printing the specification table.

- rows:

  The number of rows of the specification tibble that should be printed.

- print.specs:

  Logical value; if `TRUE`, the head of the specification tibble is
  printed.

- ...:

  further arguments passed to or from other methods (currently ignored).

## Value

Prints a summary of an object of class `specr.setup` and returns the
object invisibly.

## See also

The function
[`setup()`](https://masurp.github.io/specr/reference/setup.md), which
creates the "specr.setup" object.

## Examples

``` r
# Setup specifications
specs <- setup(data = example_data,
  x = c("x1", "x2"),
  y = c("y1", "y2"),
  model = c("lm", "glm"),
  controls = c("c1", "c2", "c3"),
  subsets = list(group3 = unique(example_data$group3)))

# Summarize specifications
summary(specs)
#> Setup for the Specification Curve Analysis
#> -------------------------------------------
#> Class:                      specr.setup -- version: 1.1.0 
#> Number of specifications:   256 
#> 
#> Specifications:
#> 
#>   Independent variable:     x1, x2 
#>   Dependent variable:       y1, y2 
#>   Models:                   lm, glm 
#>   Covariates:               no covariates, c1, c2, c3, c1 + c2, c1 + c3, c2 + c3, c1 + c2 + c3 
#>   Subsets analyses:         C, B, A, all 
#> 
#> Function used to extract parameters:
#> 
#>   function (x) 
#> broom::tidy(x, conf.int = TRUE)
#> <environment: namespace:specr>
#> 
#> 
#> Head of specifications table (first 6 rows):
#> 
#> # A tibble: 6 × 7
#>   x     y     model controls      subsets group3 formula     
#>   <chr> <chr> <chr> <chr>         <chr>   <fct>  <glue>      
#> 1 x1    y1    lm    no covariates C       C      y1 ~ x1 + 1 
#> 2 x1    y1    lm    no covariates B       B      y1 ~ x1 + 1 
#> 3 x1    y1    lm    no covariates A       A      y1 ~ x1 + 1 
#> 4 x1    y1    lm    no covariates all     NA     y1 ~ x1 + 1 
#> 5 x1    y1    lm    c1            C       C      y1 ~ x1 + c1
#> 6 x1    y1    lm    c1            B       B      y1 ~ x1 + c1
```
