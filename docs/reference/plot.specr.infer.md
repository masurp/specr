# Plot specification curve against curves under the null

This function plots the observed specification curve (estimates ranked
from lowest to highest) on top of the median and the 2.5% and 97.5%
quantiles of the ranked curves under the null (resulting from
[`infer_curve()`](https://masurp.github.io/specr/reference/infer_curve.md)).

## Usage

``` r
# S3 method for class 'specr.infer'
plot(x, ...)
```

## Arguments

- x:

  A `specr.infer` object, resulting from calling
  [`infer_curve()`](https://masurp.github.io/specr/reference/infer_curve.md).

- ...:

  further arguments passed to or from other methods (currently ignored).

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object
that can be customized further.

## See also

[`infer_curve()`](https://masurp.github.io/specr/reference/infer_curve.md),
[`summary.specr.infer()`](https://masurp.github.io/specr/reference/summary.specr.infer.md)

## Examples

``` r
specs <- setup(data = example_data,
   y = "y1",
   x = c("x1", "x2"),
   model = "lm",
   controls = c("c1", "c2"))

set.seed(42)
inference <- infer_curve(specr(specs), specs, n_samples = 200) # better 1,000 or more

# Observed curve against the curves under the null
plot(inference)
```
