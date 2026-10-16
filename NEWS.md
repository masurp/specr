# specr 1.1.0

CRAN release: 2026-10-16

## New features

* New function `infer_curve()` to conduct inference on the specification curve (the third step outlined by Simonsohn et al., 2020), based on resampling under the null. See [this vignette](https://masurp.github.io/specr/articles/inferences.html) for a comprehensive tutorial.

    - For each specification, the null is imposed on its own dependent variable, and all specifications are refitted on the same bootstrap samples of N rows, following Simonsohn et al. (2020).
    - The curves under the null are based on exactly the same specifications as the observed curve, including subsets, `simplify`, `add_to_formula`, and specifications that were filtered out after `setup()`.
    - Specifications fitted with `lm` are refitted with a fast least-squares routine (which gives the same results as `lm()`), and only the relevant parameters are extracted for other models.
    - `infer_curve()` checks that the null can be imposed (models that are linear in the dependent variable, e.g., `lm`, `lmer`, or gaussian `glm`, and numeric variables) and stops otherwise.
    - Resampling can be parallelized (via `furrr`) and is reproducible with `set.seed()`, regardless of the number of workers.

* `summary()` of the resulting `specr.infer` object reports all three test statistics proposed by Simonsohn et al. (2020): the median estimate, the number of significant specifications, and the aggregated z-value (Stouffer's Z). Tests are two-sided by default (`direction = "greater"` or `"less"` for predicted directions), p-values cannot be zero, and the significance level can be set via `alpha`. Results can be summarized by groups of specifications (`group`). `plot()` shows the observed curve against the median and the 2.5% and 97.5% quantiles of the curves under the null.

* The argument `model` of `setup()` now also accepts a named list of functions, e.g. `model = list(ols = lm, weighted = my_function)`. The names are used as labels for the models, and the functions are used directly (instead of being looked up by name, which failed e.g. for functions stored in lists; GitHub issue #29). 

* `plot()` gains the argument `choice_labels`: with `choice_labels = "left"`, the names of the analytical choices are shown on the left, next to their options (GitHub issue #39).

* `setup()` warns if an independent variable is not numeric, because no estimates can be extracted for factors or character variables (GitHub issue #47).

* New section "Other types of models" in `vignette("different-specifications")` with examples for weighted regressions (GitHub issue #27), survival models with `survival::coxph()` (#44), fixed effects models with `fixest::feols()` (#41, #43), and multilevel models with p-values from `lmerTest` (#48).

* Revised documentation: all help pages now describe their return values, `example_data` is documented in detail, and outdated or wrong statements in the vignettes were corrected (e.g., the mean index in `vignette("different-specifications")` only contained `x1`, and the custom structural equation model function did not remove the "+ 1" placeholder as described). Vignettes only evaluate their code if the suggested packages they need are installed.

## Breaking changes

* `boot_null()`, which was only part of the development version 1.0.1 on GitHub, has been removed; use `infer_curve()` instead. Results differ: `boot_null()` stacked the data of all specifications and refitted each specification on all stacked rows that contained its variables. Each specification was therefore refitted on too many rows (and partly on rows in which the null was imposed for another specification), which made the curves under the null too narrow. In simulations without any true effect, `boot_null()` rejected the null for the median in about 30% of the cases (at alpha = .05); `infer_curve()` rejects it in about 5%. P-values obtained with `boot_null()` were thus too small and should be recomputed.

* specr now requires R >= 4.1.0 (as do its dependencies).

## Bug fixes

* `specr()` stored the model names instead of the control variables in the `controls` element of the resulting `specr.object`.

* `plot(..., type = "variance")` failed when no control variables were specified, or when the specifications were filtered down to a single option of a choice ("grouping factors must have > 1 sampled level"). The variance decomposition now only includes analytical choices that have more than one option in the results.

* In `specr()`, rows with missing values in a subset variable are now excluded from the subset analyses (previously, they were passed to the model as rows of missing values, which most models dropped anyway).

* `setup()` matched control variables against `x` and `y` as text patterns, so that e.g. a control `x10` was silently dropped from all specifications with `x = "x1"`. Control variables are now matched exactly. Specifications in which `x` and `y` are the same variable are now excluded.

* `setup()` built the labels of subset combinations by removing the text "NA" from them, which garbled values containing "NA" (e.g., "DNA" became "D") and turned a value "NA" into "all". Missing values among the subset values (e.g., from `unique()`) are now ignored, and `subsets` is checked for unnamed, unknown, reserved, or duplicated entries, with informative errors instead of failures in `specr()`.

* `setup()` gives an informative error if `model` is neither a character vector nor a named list of functions.

* The model functions in the specifications kept a reference to the internal environment of `setup()`, so that saved results contained an extra copy of the data. This no longer happens, which makes results objects much smaller.

* With `fun2 = NULL`, the sample size (`fit_nobs`) is now obtained with `nobs()` instead of `broom::glance()`, so that it also works for models without a `glance()` method.

* `specr()` reported durations of more than a minute in minutes but labelled them as seconds. It now warns if specifications return no estimate for the independent variable (e.g., if `x` is a factor; GitHub issue #47) instead of silently dropping them, and if `data` is given together with a specification setup (in which case it is ignored).

* Plots failed if the results contained a column named `df` (e.g., models fitted with `lmerTest`; GitHub issue #48).

* `plot(..., type = "boxplot")`, `"variance"` and `"samplesizes"` no longer require confidence intervals, and `summary()` and `plot()` no longer fail if the results contain no sample sizes. Invalid values of `type` now give an error instead of returning `NULL`. The argument `group` of `plot()` can now also be given as a string, as in `summary()`, and grouped summaries (`summary(..., type = "curve", group = ...)`) are no longer returned as grouped tibbles.

* `print()` of a specification setup now counts the specifications after filtering, the summary of a setup always prints the head of the specifications table, and all `print()` and `summary()` methods return their input invisibly.

* The variance decomposition (`plot(..., type = "variance")` and `icc_specs()`) counted covariances of random slopes (possible with a custom `formula`) as variance components.

## Other changes

* The functions of the old framework (deprecated since version 1.0.0) keep working with current versions of ggplot2, dplyr, tidyr, and purrr (no longer using the deprecated `purrr::cross()`, which would have made specr fail to install once it is removed from purrr). Furthermore:

    - Each function now emits only one deprecation warning (instead of additional ones from deprecated functions it called internally), and the warnings name the replacement in the new framework.
    - `run_specs()` checks the confidence level also without subsets, and no longer counts duplicated rows of the data several times in combined subsets.
    - `plot_summary()` no longer merges different sets of control variables into one "all covariates" box when `all.comb = TRUE`.
    - `setup_specs()` accepts an empty vector of control variables.
    - `icc_specs()` is now formally deprecated (it was already listed as deprecated in the documentation).

* The package no longer depends on `glue` and `parallelly`.

# specr 1.0.0

CRAN release: 2023-01-20

## Breaking changes

* This new version introduces a completely new analytic framework which breaks with earlier versions of specr. See [this vignette](https://masurp.github.io/specr/articles/getting-started.html) for a comprehensive tutorial on how to use this new framework. 

* A new function called `setup()` is introduced and replaces the original `setup_specs()` to make the specification of analytical choices more intuitive and comprehensive. Most importantly, it allows to set up all specifications (now also including subset analyses, additions to the formula, etc.) a priori, i.e., before the estimation of all models. 

   - Data, analytic choices, and even parameter extraction functions can be specified before the fitting process (solving github issue #23 and #26).
   - Excludes some non-meaningful specifications automatically (e.g., when control and independent or dependent variable are the same).
   - Allows to specify all combinations of control variables or to "simplify" and only include none, each individually and all together (github issue #11).
   - Resulting specification can be filtered using standard `tidyverse` functions such as e.g., `filter()`. This way, one can make sure a priori that only reasonable specifications are actually included. 
   - Produces an object of class `specr.setup`, which can be investigated using e.g., `summary()` or `plot()`. 

* The function `run_specs()` is replaced by `specr()`, which now only wraps around `setup()` to estimate all models. Most changes are related to increasing speed of the computations. 

   - Most importantly, the estimation process can now be parallelized (based on `furrr`) to reduce fitting time (finally solving github issue #1).
   - Deals better and faster with subset analyses.
   - Produces an object of class `specr.object`, which can be investigated using generic function such as `summary()` and `plot()`. 
   - Plotting function have been updated and e.g. also allow to group specification according to specific choices (github issue #19)

* For more information about these major changes and how to use the new version of specr, see this [vignette](https://masurp.github.io/specr/articles/getting-started.html). 

* Please note that the functions from earlier versions are still available, but deprecated. It is hence still possible to use the older framework as implemented in version 0.2.1, but we suggest to move to the new framework of version 1.0.0 due to its increased speed and flexibility. 

## Known issues/bugs

* So far none...

## Acknowledgements

This version of `specr` was something I wanted to tackle for a while. Although the previous versions seem to have been embraced by the community and several relevant papers used them in interesting ways, certain issues kept coming up (ranging from slow fitting process to wanting to incorporate more complex analytical decisions and choices). This version is thus heavily inspired by the growing community of scholars who provide feedback and suggestions via [github](https://github.com/masurp/specr/issues). Thank you to all of you! But for this version, I want to thank two people particularly:

* Big thanks go to [Matti Vuorre](https://github.com/mvuorre) who explored ways to parallelize specification curve analysis in this [blogpost](https://vuorre.com/posts/parallel-multiverse/), which became the basis for the implementation in specr. 

* Many thanks to [Kasper Welbers](https://github.com/kasperwelbers) who contributed some essential code within the core function. 

* Thanks also to [Johannes Gruber](https://github.com/JBGruber) who provided support in the final stages of building this package. 


# specr 0.2.2

Development version released: 2020-12-04

## Breaking changes

* None

## Updates

* Some minor updates (related to github issues) and bug fixes:

    - All combinations of control variables can be produced (by specifying `all.comb = TRUE`; solved github issue #21)
    - `run_specs()` now allows to add sets of control variables (github issue #11)
    - All plotting functions allow to choose which parameter to plot
    - More complete results based on `broom::tidy()` and `broom::glance()`

* The package further now allows to integrate:

   - random effects (see this [vignette](https://masurp.github.io/specr/articles/multilevel-models.html))
   - latent measurement models (see this [vignette](https://masurp.github.io/specr/articles/measurement-models.html))
   
## Known issues

* Still no parallelization (github issue #1)

# specr 0.2.1

CRAN release: 2020-03-26

## Updates

* First stable version

* Tested in several environments. 

* Primary function is `run_specs()`, which allows to specify analytic choices and estimate all models across specifications. 

## Known issues

* No parallelization of the fitting process (can take very long if model fitting process is complex).

* Implementation of random effect modelling and structural equation modeling potentially possible, but still unclear. 

* Does not allow to specify all possible combinations of control variables (github issue #21). 

* Does not allow to specify sets of control variables (github issue #11)


