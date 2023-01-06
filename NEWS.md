
# specr 0.3.0

## Breaking changes

* This new version introduces a completely new analytic framework:

   - `setup_specs()` becomes `setup()` and thereby allows to completely setup all specifications before actually estimating all models. This makes the overall package way more flexible. The function `setup()` also includes new arguments that allow for more options in setting up specifications and incorporating a variety of analytical choices. 
   - `run_specs()` becomes `specr()`, which wraps around `setup()` to estimate all models. The underlying model estimation process can now be parallized to reduce fitting duration (based on `furrr`).
   - Both `setup()` and `specr()` produce S3 classes that can be investigated using generic functions such as `summary()` or `plot()`. 
   - For more information about these major changes and how to use the new version of specr, see this [vignette](https://masurp.github.io/specr/articles/specr.html). 

* Older functions from earlier versions are still available, but deprecated.

# specr 0.2.2

## Breaking changes

* None

## Updates

* Some minor updates and bug fixes:

    - all plotting functions allow to choose which parameter to plot
    - more complete results based on `broom::tidy()` and `broom::glance()`

* The package now allows to integrate:

   - random effects (see this [vignette](https://masurp.github.io/specr/articles/random_effects.html))
   - latent measurement models (see this [vignette](https://masurp.github.io/specr/articles/measurement_models.html))

# specr 0.2.1

CRAN release: 2020-03-26

* Stable version tested in several environments.

