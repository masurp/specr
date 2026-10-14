# Plot ranked specification curve

**\[deprecated\]** This function is deprecated because the new version
of specr uses a new analytic framework. In this framework, you can plot
a similar figure simply by using the generic
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) function and
adding the argument `type = "curve"`. This function plots the a ranked
specification curve. Confidence intervals can be included. Significant
results are highlighted (negative = red, positive = blue, grey =
nonsignificant). This functions creates the upper panel in
[`plot_specs()`](https://masurp.github.io/specr/reference/plot_specs.md).

## Usage

``` r
plot_curve(
  df,
  var = .data$estimate,
  group = NULL,
  desc = FALSE,
  ci = TRUE,
  ribbon = FALSE,
  legend = FALSE,
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

- desc:

  logical value indicating whether the curve should the arranged in a
  descending order. Defaults to FALSE.

- ci:

  logical value indicating whether confidence intervals should be
  plotted.

- ribbon:

  logical value indicating whether a ribbon instead should be plotted.

- legend:

  logical value indicating whether the legend should be plotted.
  Defaults to FALSE. Currently has no effect, because the colors are set
  directly and no legend is created.

- null:

  Indicate what value represents the null hypothesis (Defaults to zero)

## Value

a [ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html) object.

## Examples

``` r
# load additional library
library(ggplot2) # for further customization of the plots

# Run specification curve analysis
results <- run_specs(df = example_data,
                     y = c("y1", "y2"),
                     x = c("x1", "x2"),
                     model = c("lm"),
                     controls = c("c1", "c2"),
                     subsets = list(group1 = unique(example_data$group1),
                                    group2 = unique(example_data$group2)))

# Plot simple specification curve
plot_curve(results)
#> Warning: `plot_curve()` was deprecated in specr 1.0.0.
#> ℹ Please use `plot(<specr.object>, type = "curve")` instead.


# Ribbon instead of CIs and customize further
plot_curve(results, ci = FALSE, ribbon = TRUE) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = median(results$estimate),
             linetype = "dashed") +
  theme_linedraw()
```
