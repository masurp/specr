# Plot how analytical choices affect results

**\[deprecated\]** This function is deprecated because the new version
of specr uses a new analytic framework. In this framework, you can plot
a similar figure simply by using the generic
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) function and
adding the argument `type = "choices"`. This functions plots how
analytic choices affect the obtained results (i.e., the rank within the
curve). Significant results are highlighted (negative = red, positive =
blue, grey = nonsignificant). This functions creates the lower panel in
[`plot_specs()`](https://masurp.github.io/specr/reference/plot_specs.md).

## Usage

``` r
plot_choices(
  df,
  var = .data$estimate,
  group = NULL,
  choices = c("x", "y", "model", "controls", "subsets"),
  desc = FALSE,
  null = 0
)
```

## Arguments

- df:

  a data frame resulting from
  [`run_specs()`](https://masurp.github.io/specr/reference/run_specs.md).

- var:

  which variable should be evaluated? Defaults to estimate (the effect
  sizes computed by
  [`run_specs()`](https://masurp.github.io/specr/reference/run_specs.md)).

- group:

  Should the arrangement of the curve be grouped by a particular choice?
  Defaults to NULL, but can be any of the present choices (e.g., x, y,
  controls...)

- choices:

  a vector specifying which analytical choices should be plotted. By
  default, all choices are plotted.

- desc:

  logical value indicating whether the curve should the arranged in a
  descending order. Defaults to FALSE.

- null:

  Indicate what value represents the 'null' hypothesis (Defaults to
  zero).

## Value

a [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Examples

``` r
# Run specification curve analysis
results <- run_specs(df = example_data,
                     y = c("y1", "y2"),
                     x = c("x1", "x2"),
                     model = c("lm"),
                     controls = c("c1", "c2"),
                     subsets = list(group1 = unique(example_data$group1),
                                    group2 = unique(example_data$group2)))

# Plot simple table of choices
plot_choices(results)
#> Warning: `plot_choices()` was deprecated in specr 1.0.0.
#> ℹ Please use `plot(<specr.object>, type = "choices")` instead.


# Plot only specific choices
plot_choices(results,
             choices = c("x", "y", "controls"))
```
