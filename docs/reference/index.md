# Package index

## Conduct specification curve analysis

Core functions of the package that allow to a) setup all specifications,
b) fit models across all specifications, and c) conduct inference on the
resulting specification curve

- [`setup()`](https://masurp.github.io/specr/reference/setup.md) :
  Specifying analytical decisions in a specification setup
- [`specr()`](https://masurp.github.io/specr/reference/specr.md) : Fit
  models across all specifications
- [`infer_curve()`](https://masurp.github.io/specr/reference/infer_curve.md)
  : Inference on the specification curve

## Summarize results

All functions above create S3 classes that can be investigated with the
generic summary function and transformed to data frames.

- [`summary(`*`<specr.setup>`*`)`](https://masurp.github.io/specr/reference/summary.specr.setup.md)
  : Summarizing the Specifications Setup
- [`summary(`*`<specr.object>`*`)`](https://masurp.github.io/specr/reference/summary.specr.object.md)
  : Summarizing the Specification Curve Analysis
- [`summary(`*`<specr.infer>`*`)`](https://masurp.github.io/specr/reference/summary.specr.infer.md)
  : Summarizing inference on the specification curve

## Visualizations

After running setup(), specr(), or infer_curve(), the resulting S3 class
objects can be passed to the generic plot function to create
visualizations.

- [`plot(`*`<specr.setup>`*`)`](https://masurp.github.io/specr/reference/plot.specr.setup.md)
  : Plot visualization of the specification setup
- [`plot(`*`<specr.object>`*`)`](https://masurp.github.io/specr/reference/plot.specr.object.md)
  : Plot specification curve and analytic choices
- [`plot(`*`<specr.infer>`*`)`](https://masurp.github.io/specr/reference/plot.specr.infer.md)
  : Plot specification curve against curves under the null

## Data set(s)

- [`example_data`](https://masurp.github.io/specr/reference/example_data.md)
  : Example data set

## Deprecated

The following functions still exist and can be used, but are deprecated
from version 1.0.0 on.

- [`setup_specs()`](https://masurp.github.io/specr/reference/setup_specs.md)
  **\[deprecated\]** : Set up specifications
- [`run_specs()`](https://masurp.github.io/specr/reference/run_specs.md)
  **\[deprecated\]** : Estimate all specifications
- [`plot_specs()`](https://masurp.github.io/specr/reference/plot_specs.md)
  **\[deprecated\]** : Plot specification curve and analytical choices
- [`plot_curve()`](https://masurp.github.io/specr/reference/plot_curve.md)
  **\[deprecated\]** : Plot ranked specification curve
- [`plot_choices()`](https://masurp.github.io/specr/reference/plot_choices.md)
  **\[deprecated\]** : Plot how analytical choices affect results
- [`plot_summary()`](https://masurp.github.io/specr/reference/plot_summary.md)
  **\[deprecated\]** : Create box plots for given analytical choices
- [`plot_samplesizes()`](https://masurp.github.io/specr/reference/plot_samplesizes.md)
  **\[deprecated\]** : Plot sample sizes
- [`plot_variance()`](https://masurp.github.io/specr/reference/plot_variance.md)
  **\[deprecated\]** : Plot variance decomposition
- [`plot_decisiontree()`](https://masurp.github.io/specr/reference/plot_decisiontree.md)
  **\[deprecated\]** : Plot decision tree
- [`summarise_specs()`](https://masurp.github.io/specr/reference/summarise_specs.md)
  **\[deprecated\]** : Summarise specifications
- [`icc_specs()`](https://masurp.github.io/specr/reference/icc_specs.md)
  **\[deprecated\]** : Compute intraclass correlation coefficient
