# Plot visualization of the specification setup

This function plots a visual summary of the specification setup. It
requires an object of class `specr.setup`, usually the result of calling
[`setup()`](https://masurp.github.io/specr/reference/setup.md).

## Usage

``` r
# S3 method for class 'specr.setup'
plot(x, layout = "dendrogram", circular = FALSE, ...)
```

## Arguments

- x:

  A `specr.setup` object, usually resulting from calling
  [`setup()`](https://masurp.github.io/specr/reference/setup.md).

- layout:

  The type of layout to create for the garden of forking path. Defaults
  to "dendrogram". See `?ggraph` for options.

- circular:

  Should the layout be transformed into a radial representation. Only
  possible for some layouts. Defaults to FALSE.

- ...:

  further arguments passed to or from other methods (currently ignored).

## Value

A [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object
that can be customized further.

## Examples

``` r
# \donttest{
specs <- setup(data = example_data,
   x = c("x1", "x2", "x3"),
   y = c("y1", "y2"),
   model = c("lm", "glm"),
   controls = "c1",
   subsets = list(group2 = unique(example_data$group2)))

plot(specs)

plot(specs, circular = TRUE)

# }
```
