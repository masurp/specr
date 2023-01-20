
# specr 1.0.0

CRAN release: [soon]

## Breaking changes

* This new version introduces a completely new analytic framework which breaks with earlier versions of specr. See [this vignette](https://masurp.github.io/specr/articles/specr.html) for a comprehensive tutorial on how to use this new framework. 

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

* For more information about these major changes and how to use the new version of specr, see this [vignette](https://masurp.github.io/specr/articles/specr.html). 

* Please note that the functions from earlier versions are still available, but deprecated. It is hence still possible to use the older framework as implemented in version 0.2.1, but we suggest to move to the new framework of version 0.3.0 due to its increased speed and flexibility. 

## Known issues/bugs

* So far none...

## Acknowledgements

This version of `specr` was something I wanted to tackle for a while. Although the previous versions seem to have embraced by the community and several relevant papers used them in interesting ways, certain issues kept coming up (ranging from slow fitting process to wanting to incorporate more complex analytical decisions and choices). This version is thus heavily inspired by the growing community of scholars who provide feedback and suggestions via [github](https://github.com/masurp/specr/issues). Thank you to all of you! But for this version, I want to thank two people particularly:

* Big thanks go to [Matti Vuorre](https://github.com/mvuorre) who explored ways to parallelize specification curve analysis in this [blogpost](https://vuorre.netlify.app/posts/parallel-multiverse/), which became the basis for the implementation in specr. 

* Many thanks to [Kasper Welbers](https://github.com/kasperwelbers) who contributed some essential code within the core function. 

* Thanks also to [Johannes Gruber](https://github.com/JBGruber) who provided support in the final stages of building this package. 


# specr 0.2.2

Development version released: 2020-12-04

## Breaking changes

* None

## Updates

* Some minor updates (related to gituhub issues) and bug fixes:

    - All combinations of control variables can be produced (by specifying `all.comb = TRUE`; solved github issue #21)
    - `run_specs()` now allows to add sets of control variabels (github issue #11)
    - All plotting functions allow to choose which parameter to plot
    - More complete results based on `broom::tidy()` and `broom::glance()`

* The package further now allows to integrate:

   - random effects (see this [vignette](https://masurp.github.io/specr/articles/random_effects.html))
   - latent measurement models (see this [vignette](https://masurp.github.io/specr/articles/measurement_models.html))
   
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


