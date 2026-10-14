# Specifying analytical decisions in a specification setup

Creates all possible specifications as a combination of different
dependent and independent variables, model types, control variables,
potential subset analyses, as well as potentially other analytic
choices. This function represents the first step in the analytic
framework implemented in the package `specr`. The resulting class
`specr.setup` then needs to be passed to the core function of the
package called
[`specr()`](https://masurp.github.io/specr/reference/specr.md), which
fits the specified models across all specifications.

## Usage

``` r
setup(
  data,
  x,
  y,
  model,
  controls = NULL,
  subsets = NULL,
  add_to_formula = NULL,
  fun1 = function(x) broom::tidy(x, conf.int = TRUE),
  fun2 = function(x) broom::glance(x),
  simplify = FALSE
)
```

## Arguments

- data:

  The data set that should be used for the analysis

- x:

  A character vector denoting the independent variables (which should be
  numeric variables, see details)

- y:

  A character vector denoting the dependent variables

- model:

  A character vector with the names of the model functions that should
  be estimated (e.g., `"lm"` or the name of a custom function), or a
  named list of functions (e.g., `list(ols = lm, robust = MASS::rlm)`),
  in which case the names are used as labels for the models. Any
  function that accepts the arguments `formula` and `data` can be used,
  including custom wrappers around other model functions (e.g., for
  survival models, fixed effects models, or weighted regressions; see
  [`vignette("different-specifications")`](https://masurp.github.io/specr/articles/different-specifications.md)).

- controls:

  A vector of the control variables that should be included. Defaults to
  NULL.

- subsets:

  Specification of potential subsets/groups as list. There are two ways
  in which these can be specified that both start from the assumption
  that the "grouping" variable is in the data set. The simplest way is
  to provide a named vector within the list, whose name is the variable
  that should be used for subsetting and whose values are the values
  that reflect the subsets (e.g., `list(group2 = c("female", "male"))`).
  In this case, the specifications will include "all", "only female" and
  "only male". Alternatively, you can also use the `unique` function to
  extract that vector directly from the data set (e.g.,
  `list(group2 = unique(example_data$group2))`). Both approaches lead to
  the same result. The former, however, has the advantages that one can
  also remove some of the subgroups (e.g. `list(group2 = c("female"))`).
  In this case, the specifications will include "all" (no subset) and
  "only females". See examples for more details. Missing values (NA)
  among the subset values are ignored.

- add_to_formula:

  A string specifying aspects that should always be included in the
  formula (e.g. a constant covariate, random effect structures...)

- fun1:

  A function that extracts the parameters of interest from the fitted
  models. Defaults to
  [tidy](https://broom.tidymodels.org/reference/broom.html), which works
  with a large range of different models.

- fun2:

  A function that extracts fit indices of interest from the models.
  Defaults to
  [glance](https://broom.tidymodels.org/reference/broom.html), which
  works with a large range of different models. Note: Different models
  result in different fit indices. Thus, if you use different models
  within one specification curve analysis, this may not work. In this
  case, you can simply set `fun2 = NULL` to not extract any fit indices.

- simplify:

  Logical value indicating what type of combinations between control
  variables should be included in the specification. If FALSE (default),
  all combinations between the provided variables are created (none,
  each individually, each combination between each variable, all
  variables). If TRUE, only no covariates, each individually, and all
  covariates are included as specifications (akin to the default in
  specr version 0.2.1).

## Value

An object of class `specr.setup` which includes all possible
specifications based on combinations of the analytic choices. The
resulting list includes a specification tibble, the data set, and
additional information about the universe of specifications. Use
`methods(class = "specr.setup")` for an overview on available methods.

## Details

Empirical results are often contingent on analytical decisions that are
equally defensible, often arbitrary, and motivated by different reasons.
These decisions may introduce bias or at least variability. To this end,
specification curve analyses (Simonsohn et al., 2020) or multiverse
analyses (Steegen et al., 2016) refer to identifying the set of
theoretically justified, statistically valid (and potentially also
non-redundant) specifications, fitting the "multiverse" of models
represented by these specifications and extract relevant parameters
often to display the results graphically as a so-called specification
curve. This allows readers to identify consequential specifications
decisions and how they affect the results or parameter of interest.

**Use of this function**

A general overview is provided in the vignette
[`vignette("getting-started")`](https://masurp.github.io/specr/articles/getting-started.md).
It is assumed that you want to estimate the relationship between two
variables (`x` and `y`). What varies may be what variables should be
used for `x` and `y`, what model should be used to estimate the
relationship, whether the relationship should be estimated for certain
subsets, and whether different combinations of control variables should
be included. This allows to (re)produce almost any analytical decision
imaginable. See examples below for how a number of typical analytical
decision can be implemented. Afterwards you pass the resulting object of
a class `specr.setup` to the function
[`specr()`](https://masurp.github.io/specr/reference/specr.md) to run
the specification curve analysis.

As [`specr()`](https://masurp.github.io/specr/reference/specr.md)
extracts the estimate whose term equals `x`, the independent variables
should be numeric. For factor or character variables, models return
terms such as "xB", so recode them as (dummy) variables instead.

Note, the resulting class of `specr.setup` allows to use generic
functions. Use `methods(class = "specr.setup")` for an overview on
available methods and e.g.,
[`?summary.specr.setup`](https://masurp.github.io/specr/reference/summary.specr.setup.md)
to view the dedicated help page.

## References

- Simonsohn, U., Simmons, J.P. & Nelson, L.D. (2020). Specification
  curve analysis. *Nature Human Behaviour, 4*, 1208–1214.
  https://doi.org/10.1038/s41562-020-0912-z

- Steegen, S., Tuerlinckx, F., Gelman, A., & Vanpaemel, W. (2016).
  Increasing Transparency Through a Multiverse Analysis. *Perspectives
  on Psychological Science, 11*(5), 702-712.
  https://doi.org/10.1177/1745691616658637

## See also

[`specr()`](https://masurp.github.io/specr/reference/specr.md) for the
second step of actually running the actual specification curve analysis

[`summary.specr.setup()`](https://masurp.github.io/specr/reference/summary.specr.setup.md)
for how to summarize and inspect the resulting specifications

[`plot.specr.setup()`](https://masurp.github.io/specr/reference/plot.specr.setup.md)
for creating a visual summary of the specification setup.

## Examples

``` r
## Example 1 ----
# Setting up typical specifications
specs <- setup(data = example_data,
   x = c("x1", "x2"),
   y = c("y1", "y2"),
   model = "lm",
   controls = c("c1", "c2", "c3"),
   subsets = list(group1 = c("young", "middle", "old"),
                  group2 = c("female", "male")),
   simplify = TRUE)

# Check specifications
summary(specs, rows = 18)
#> Setup for the Specification Curve Analysis
#> -------------------------------------------
#> Class:                      specr.setup -- version: 1.1.0 
#> Number of specifications:   240 
#> 
#> Specifications:
#> 
#>   Independent variable:     x1, x2 
#>   Dependent variable:       y1, y2 
#>   Models:                   lm 
#>   Covariates:               no covariates, c1, c2, c3, all covariates 
#>   Subsets analyses:         young & female, middle & female, old & female, female, young & male, middle & male, old & male, male, young, middle, old, all 
#> 
#> Function used to extract parameters:
#> 
#>   function (x) 
#> broom::tidy(x, conf.int = TRUE)
#> <environment: namespace:specr>
#> 
#> 
#> Head of specifications table (first 18 rows):
#> 
#> # A tibble: 18 × 8
#>    x     y     model controls      subsets         group1 group2 formula     
#>    <chr> <chr> <chr> <chr>         <chr>           <fct>  <fct>  <glue>      
#>  1 x1    y1    lm    no covariates young & female  young  female y1 ~ x1 + 1 
#>  2 x1    y1    lm    no covariates middle & female middle female y1 ~ x1 + 1 
#>  3 x1    y1    lm    no covariates old & female    old    female y1 ~ x1 + 1 
#>  4 x1    y1    lm    no covariates female          NA     female y1 ~ x1 + 1 
#>  5 x1    y1    lm    no covariates young & male    young  male   y1 ~ x1 + 1 
#>  6 x1    y1    lm    no covariates middle & male   middle male   y1 ~ x1 + 1 
#>  7 x1    y1    lm    no covariates old & male      old    male   y1 ~ x1 + 1 
#>  8 x1    y1    lm    no covariates male            NA     male   y1 ~ x1 + 1 
#>  9 x1    y1    lm    no covariates young           young  NA     y1 ~ x1 + 1 
#> 10 x1    y1    lm    no covariates middle          middle NA     y1 ~ x1 + 1 
#> 11 x1    y1    lm    no covariates old             old    NA     y1 ~ x1 + 1 
#> 12 x1    y1    lm    no covariates all             NA     NA     y1 ~ x1 + 1 
#> 13 x1    y1    lm    c1            young & female  young  female y1 ~ x1 + c1
#> 14 x1    y1    lm    c1            middle & female middle female y1 ~ x1 + c1
#> 15 x1    y1    lm    c1            old & female    old    female y1 ~ x1 + c1
#> 16 x1    y1    lm    c1            female          NA     female y1 ~ x1 + c1
#> 17 x1    y1    lm    c1            young & male    young  male   y1 ~ x1 + c1
#> 18 x1    y1    lm    c1            middle & male   middle male   y1 ~ x1 + c1


## Example 2 ----
# Setting up specifications for multilevel models
specs <- setup(data = example_data,
   x = c("x1", "x2"),
   y = c("y1", "y2"),
   model = c("lmer"),                                   # multilevel model
   subsets = list(group1 = c("young", "old"),           # only young and old!
                  group2 = unique(example_data$group2)),# alternative specification
   controls = c("c1", "c2"),
   add_to_formula = "(1|group2)")                       # random effect in all models

# Check specifications
summary(specs)
#> Setup for the Specification Curve Analysis
#> -------------------------------------------
#> Class:                      specr.setup -- version: 1.1.0 
#> Number of specifications:   144 
#> 
#> Specifications:
#> 
#>   Independent variable:     x1, x2 
#>   Dependent variable:       y1, y2 
#>   Models:                   lmer 
#>   Covariates:               no covariates, c1, c2, c1 + c2 
#>   Subsets analyses:         young & female, old & female, female, young & male, old & male, male, young, old, all 
#> 
#> Function used to extract parameters:
#> 
#>   function (x) 
#> broom::tidy(x, conf.int = TRUE)
#> <environment: namespace:specr>
#> 
#> 
#> Head of specifications table (first 6 rows):
#> 
#> # A tibble: 6 × 8
#>   x     y     model controls      subsets        group1 group2 formula          
#>   <chr> <chr> <chr> <chr>         <chr>          <fct>  <fct>  <glue>           
#> 1 x1    y1    lmer  no covariates young & female young  female y1 ~ x1 + 1 + (1…
#> 2 x1    y1    lmer  no covariates old & female   old    female y1 ~ x1 + 1 + (1…
#> 3 x1    y1    lmer  no covariates female         NA     female y1 ~ x1 + 1 + (1…
#> 4 x1    y1    lmer  no covariates young & male   young  male   y1 ~ x1 + 1 + (1…
#> 5 x1    y1    lmer  no covariates old & male     old    male   y1 ~ x1 + 1 + (1…
#> 6 x1    y1    lmer  no covariates male           NA     male   y1 ~ x1 + 1 + (1…


## Example 3 ----
# Setting up specifications with a different parameter extract functions

# Create custom extract function to extract different parameter and model
tidy_99 <- function(x) {
  fit <- broom::tidy(x,
     conf.int = TRUE,
     conf.level = .99)         # different alpha error rate
  fit$full_model = list(x)     # include entire model fit object as list
  return(fit)
}

# Setup specs
specs <- setup(data = example_data,
   x = c("x1", "x2"),
   y = c("y1", "y2"),
   model = "lm",
   fun1 = tidy_99,             # pass new function to setup
   add_to_formula = "c1 + c2") # set of covariates in all models

# Check specifications
summary(specs)
#> Setup for the Specification Curve Analysis
#> -------------------------------------------
#> Class:                      specr.setup -- version: 1.1.0 
#> Number of specifications:   4 
#> 
#> Specifications:
#> 
#>   Independent variable:     x1, x2 
#>   Dependent variable:       y1, y2 
#>   Models:                   lm 
#>   Covariates:               no covariates 
#>   Subsets analyses:         all 
#> 
#> Function used to extract parameters:
#> 
#>   function (x) 
#> {
#>     fit <- broom::tidy(x, conf.int = TRUE, conf.level = 0.99)
#>     fit$full_model = list(x)
#>     return(fit)
#> }
#> <environment: 0x74f96324d8>
#> 
#> 
#> Head of specifications table (first 6 rows):
#> 
#> # A tibble: 4 × 6
#>   x     y     model controls      subsets formula              
#>   <chr> <chr> <chr> <chr>         <chr>   <glue>               
#> 1 x1    y1    lm    no covariates all     y1 ~ x1 + 1 + c1 + c2
#> 2 x1    y2    lm    no covariates all     y2 ~ x1 + 1 + c1 + c2
#> 3 x2    y1    lm    no covariates all     y1 ~ x2 + 1 + c1 + c2
#> 4 x2    y2    lm    no covariates all     y2 ~ x2 + 1 + c1 + c2


## Example 4 ----
# Model functions as a named list (the names are used as labels)

# Custom function, e.g. a weighted regression (any function with the
# arguments `formula` and `data` works, e.g. also wrappers around
# fixest::feols() or survival::coxph(), see vignette("different-specifications"))
weighted_lm <- function(formula, data) {
  lm(formula, data = data, weights = rep(1:2, length.out = nrow(data)))
}

specs <- setup(data = example_data,
   x = c("x1", "x2"),
   y = "y1",
   model = list(ols = lm, weighted = weighted_lm),
   controls = "c1")

# Check specifications
summary(specs)
#> Setup for the Specification Curve Analysis
#> -------------------------------------------
#> Class:                      specr.setup -- version: 1.1.0 
#> Number of specifications:   8 
#> 
#> Specifications:
#> 
#>   Independent variable:     x1, x2 
#>   Dependent variable:       y1 
#>   Models:                   ols, weighted 
#>   Covariates:               no covariates, c1 
#>   Subsets analyses:         all 
#> 
#> Function used to extract parameters:
#> 
#>   function (x) 
#> broom::tidy(x, conf.int = TRUE)
#> <environment: namespace:specr>
#> 
#> 
#> Head of specifications table (first 6 rows):
#> 
#> # A tibble: 6 × 6
#>   x     y     model    controls      subsets formula     
#>   <chr> <chr> <chr>    <chr>         <chr>   <glue>      
#> 1 x1    y1    ols      no covariates all     y1 ~ x1 + 1 
#> 2 x1    y1    ols      c1            all     y1 ~ x1 + c1
#> 3 x1    y1    weighted no covariates all     y1 ~ x1 + 1 
#> 4 x1    y1    weighted c1            all     y1 ~ x1 + c1
#> 5 x2    y1    ols      no covariates all     y1 ~ x2 + 1 
#> 6 x2    y1    ols      c1            all     y1 ~ x2 + c1
```
