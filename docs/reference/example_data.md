# Example data set

This simulated data set can be used to explore the major function of
'specr'. It provides variables that can be used to mimic different
independent and dependent variables, control variables, and grouping
variables (for subset analyses).

## Usage

``` r
data(example_data)
```

## Format

A tibble with 1,000 rows and 15 variables (no missing values):

- group1:

  Grouping variable with three groups ("young", "middle", "old"), e.g.
  for subset analyses.

- group2:

  Grouping variable with two groups ("female", "male").

- group3:

  Grouping variable with three groups ("A", "B", "C").

- x1, x2, x3, x4:

  Numeric variables that can be used as (alternative) independent
  variables.

- c1, c2, c3, c4:

  Numeric variables that can be used as control variables.

- y1, y2, y3, y4:

  Numeric variables that can be used as (alternative) dependent
  variables.

## Source

Simulated data.

## Examples

``` r
data(example_data)
head(example_data)
#> # A tibble: 6 × 15
#>   group1 group2 group3     x1      x2      x3     x4     c1      c2     c3
#>   <chr>  <chr>  <chr>   <dbl>   <dbl>   <dbl>  <dbl>  <dbl>   <dbl>  <dbl>
#> 1 middle female C       1.17  -0.363  -0.999  -0.293  0.736  0.171   0.244
#> 2 middle male   B       0.765  0.0349  0.129  -0.155 -0.720  0.0165 -0.771
#> 3 old    female A      -2.33  -0.998  -1.11   -1.27   0.104 -2.90   -6.95 
#> 4 old    female B      -0.507 -0.970  -0.0867 -0.978  1.79  -0.138  -0.431
#> 5 old    male   A      -0.999  0.546   0.901   2.57  -0.245  2.02    2.83 
#> 6 middle male   A      -0.177 -1.37    0.0947 -2.53   0.315 -0.688  -1.33 
#> # ℹ 5 more variables: c4 <dbl>, y1 <dbl>, y2 <dbl>, y3 <dbl>, y4 <dbl>
```
