
# specr 0.3.0

CRAN release: [soon]

## Breaking changes

* This new version introduces a completely new analytic framework which breaks with earlier version of specr

* A new function called `setup()` is introduced and replaces the original `setup_specs()` to make the specification of analytical choices more intuitive and comprehensive. Most importantly, it allows to set up all specifications (now also including subset analyses, additions to the formula, etc.) a priori, i.e., before the estimation of all models . 

   - Data, analytic choices, and even parameter extraction functions can be specified before the fitting process (solving github issue #26).
   - Excludes non-meaningful specifications (e.g., when control and independent or dependent variable are the same).
   - Allows to specify all combinations of control variables or to "simplify" and only include none, each individually and all together (github issue #11).
   - Resulting specification can be filtered using standard `tidyverse` functions such as e.g., `filter()`. This way, one can make sure a priori that only reasonable specifications are actually included. 
   - Produces an object of class `specr.setup`, which can be investigated using e.g., `summary()`. 

* The function `run_specs()` is replaced by `specr()`, which now only wraps around `setup()` to estimate all models. Most changes are related to increasing speed of the computations. 

   - Most importantly, the estimation process can now be parallelized (based on `furrr`, finally solving github issue #1) to reduce fitting time. 
   - Produces an object of class `specr.object`, which can be investigated using generic function such as `summary()` and `plot(). 

* For more information about these major changes and how to use the new version of specr, see this [vignette](https://masurp.github.io/specr/articles/specr.html). 

* Please note that the functions from earlier versions are still available, but deprecated. It is hence still possible to use the older framework as implemented in version 0.2.1, but we suggest to move to the new framework of version 0.3.0 due to its increased speed and flexibility. 

## Known issues/bugs

* So far none...

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


# Pre-release version

* This version is still in development but main functions and features are established. 

## Known issues

* Some further performance enhancements, customizations, debugging, and cosmetic changes will take place before any official release.
