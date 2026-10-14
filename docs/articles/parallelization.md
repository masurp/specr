# Using parallelization

A major improvement in version 1.0.0 of specr is that we can parallelize
the computations, which can reduce fitting time. For this, specr uses
functions from the package [furrr](https://furrr.futureverse.org/). I
suggest to check out the website of the package for further information.
To be able to use the relevant functions, we load the package furrr
upfront.

Before we start to run some examples, bear in mind that using
parallelization does not always mean that the computations are
automatically faster. If the overall computation time is not very long,
using parallelization may even lead to a longer fitting process as
setting up several “workers” (essentially setting up procedures on
several cores) produces a considerable “overhead”. A true reduction in
fitting time is thus only achieved when the data set is large, the
number of specifications is high, and the computed models are complex. I
thus first simulate a comparatively large data set that allows to
specify more than 1000 specifications.

\
`# Load packages`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyverse`](https://tidyverse.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`specr`](https://masurp.github.io/specr/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`furrr`](https://github.com/futureverse/furrr)`)`\
\
`# Data generating function`\
`generate_data`` ``<-`` ``function``(``seed`` ``=`` ``42``, ``n`` ``=`` ``1e5``)`` ``{`\
`  ``if`` ``(``!`[`is.na`](https://rdrr.io/r/base/NA.html)`(``seed``)``)`` `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``seed``)`\
`  ``dat`` ``<-`` `[`tibble`](https://tibble.tidyverse.org/reference/tibble.html)`(`\
`    x1 ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)``,`\
`    x2 ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)``+`` ``0.9``*``x1``,`\
`    x3 ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)``+`` ``0.9``*``x2``,`\
`    x4 ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)``+`` ``0.9``*``x3``,`\
`    y4 ``=`` `[`rep`](https://rdrr.io/r/base/rep.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``1``, ``0``)``, times ``=`` ``n``/``2``)``,`\
`    y1 ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)`` ``+`` ``x1``*``.1`` ``*`` ``0.9``*``y4``,`\
`    y2 ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)`` ``+`` ``x1``*``.2``,`\
`    y3 ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)`` ``+`` ``x1``*``.2`` ``+`` ``-``0.4``*``x2``, `\
`    c1 ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)`` ``+`` ``x1``*``.3``,`\
`    c2 ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)``,`\
`    c3 ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)`` ``+`` ``0.9``*``c1``,`\
`    c4 ``=`` `[`rnorm`](https://rdrr.io/r/stats/Normal.html)`(``n``)``,`\
`    group ``=`` `[`sample`](https://rdrr.io/r/base/sample.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"a"``, ``"b"``, ``"c"``, ``"d"``, ``"e"``)``, ``n``, replace ``=`` ``TRUE``)`\
`  ``)`\
`}`\
\
`# Generate very large data set (n = 100,000, 9.9 MB on disk)`\
`dat`` ``<-`` ``generate_data``(``9``)`

## Simple parallelization without custom functions

If we use standard model fitting functions (e.g., “lm”) that are
included in base R, parallelization is comparatively simple. We only
need to load the package `furrr` and specify a “plan for how to resolve
a future” (for more information see
[`?future::plan`](https://future.futureverse.org/reference/plan.html)).
In this case, I am choosing `multisession` (resolve the computations in
separate R sessions running in the background on the same machine) and
specify `workers = 4` so that it runs on 4 cores in parallel.

\
`# Setup of specifications (number of specs = 1152)`\
`specs`` ``<-`` `[`setup`](https://masurp.github.io/specr/reference/setup.md)`(``data ``=`` ``dat``,            `\
`               y ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"y1"``, ``"y2"``, ``"y3"``)``,                    `\
`               x ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"x1"``, ``"x2"``, ``"x3"``, ``"x4"``)``,               `\
`               model ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"lm"``)``, `\
`               controls ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"c1"``, ``"c2"``, ``"c3"``, ``"c4"``)``,`\
`               subsets ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``group ``=`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``dat``$``group``)``)``)``   `\
\
`# Default: Sequential ---`\
`results_simple`` ``<-`` `[`specr`](https://masurp.github.io/specr/reference/specr.md)`(``specs``)`\
\
`` # Parallel: Multisession (only works when `furrr` is loaded!) ``\
[`plan`](https://future.futureverse.org/reference/plan.html)`(``strategy ``=`` ``multisession``, workers ``=`` ``4``)`\
`results_parall`` ``<-`` `[`specr`](https://masurp.github.io/specr/reference/specr.md)`(``specs``)`\
\
`# Comparison`\
[`cat`](https://rdrr.io/r/base/cat.html)`(``"Sequential: "``, ``results_simple``$``time``, ``"\n"``,`\
`    ``"Parallel:   "``, ``results_parall``$``time``)`

    ## Sequential:  57.227 sec elapsed 
    ## Parallel:    36.781 sec elapsed

As we can see, the default (sequential) computation took around a minute
(57.227 sec elapsed) and the parallel computation about half a minute
(36.781 sec elapsed).

We have to acknowledge that even with this comparatively large data set
and more than 1,000 specifications, the reduction in time is not very
large. Thus, parallelization only makes sense if you have a truly large
data set and thousands of specifications *or* the type of model you are
estimating takes a really long time (e.g., a complex structural equation
model, a negative binomial model, etc.). The true power of
parallelization furthermore only comes into play if you are using a
considerable number of cores.

## Parallelization with custom functions from different packages

If we use a custom function, we need to make sure that this function is
passed to the different workers. This can be done by specifying so
called `furrr_options`. We need to pass objects from the global
environment (and potentially also packages) as shown below. Please note
that we do not have to specify the “future plan” again, because we have
specified it already earlier in this session (see above). If we are
unsure what plan is currently specified, we can simply run
[`plan()`](https://future.futureverse.org/reference/plan.html) and get
some information about the current setup.

As computation can take a long time, it would be nice if we would see
some kind of progress indication. This can easily be done by simply
adding the argument `.progress = TRUE` to
[`specr()`](https://masurp.github.io/specr/reference/specr.md). This
passes this argument to the
[`future_pmap()`](https://furrr.futureverse.org/reference/future_map2.html)
function within
[`specr()`](https://masurp.github.io/specr/reference/specr.md) and
prints a rudimentary progress bar during the fitting process.

\
`# Custom function`\
`log_model`` ``<-`` ``function``(``formula``, ``data``)`` ``{`\
`  `[`glm`](https://rdrr.io/r/stats/glm.html)`(``formula``, ``data``, family ``=`` `[`binomial`](https://rdrr.io/r/stats/family.html)`(``)``)`\
`}`\
\
`# Setup specs`\
`specs`` ``<-`` `[`setup`](https://masurp.github.io/specr/reference/setup.md)`(``data ``=`` ``dat``,            `\
`               y ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"y4"``)``,                    `\
`               x ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"x1"``, ``"x2"``, ``"x3"``)``,               `\
`               model ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"log_model"``)``, `\
`               controls ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"c1"``, ``"c2"``)``,`\
`               subsets ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``group ``=`` `[`unique`](https://rdrr.io/r/base/unique.html)`(``dat``$``group``)``)``)``   `\
\
`` # Create furrr_options to be passed to specr() (only works if `furrr` is loaded) ``\
`opts`` ``<-`` `[`furrr_options`](https://furrr.futureverse.org/reference/furrr_options.html)`(`\
`  globals ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``log_model ``=`` ``log_model``)`\
`)`\
\
`# What "plan" is currently specified?`\
[`plan`](https://future.futureverse.org/reference/plan.html)`(``)`

    ## multisession:
    ## - args: function (..., workers = 4, envir = parent.frame())
    ## - tweaked: TRUE
    ## - call: plan(strategy = multisession, workers = 4)

\
`# Run results`\
`results_parall_2`` ``<-`` `[`specr`](https://masurp.github.io/specr/reference/specr.md)`(``specs``, `\
`                          .options ``=`` ``opts``,   ``# Pass ops to specr`\
`                          .progress ``=`` ``TRUE``)``  ``# To add progress bar (not shown in output)`\
\
`# Summarize results`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``results_parall_2``)`

    ## Results of the specification curve analysis
    ## -------------------
    ## Technical details:
    ## 
    ##   Class:                          specr.object -- version: 1.0.0 
    ##   Cores used:                     4 
    ##   Duration of fitting process:    37.736 sec elapsed 
    ##   Number of specifications:       72 
    ## 
    ## Descriptive summary of the specification curve:
    ## 
    ##  median  mad   min  max q25  q75
    ##    0.01 0.01 -0.02 0.03   0 0.01
    ## 
    ## Descriptive summary of sample sizes: 
    ## 
    ##  median   min   max
    ##   20038 19844 1e+05
    ## 
    ## Head of the specification results (first 6 rows): 
    ## 
    ## # A tibble: 6 × 21
    ##   x     y     model  controls subsets group formula estimate std.error statistic p.value conf.low 
    ##   <chr> <chr> <chr>  <chr>    <chr>   <fct> <glue>     <dbl>     <dbl>     <dbl>   <dbl>    <dbl> 
    ## 1 x1    y4    log_m… no cova… e       e     y4 ~ x…     0.01      0.01      0.38    0.7     -0.02 
    ## 2 x1    y4    log_m… no cova… a       a     y4 ~ x…     0.01      0.01      0.62    0.54    -0.02 
    ## 3 x1    y4    log_m… no cova… d       d     y4 ~ x…    -0.02      0.01     -1.17    0.24    -0.04 
    ## 4 x1    y4    log_m… no cova… c       c     y4 ~ x…     0         0.01     -0.16    0.87    -0.03 
    ## 5 x1    y4    log_m… no cova… b       b     y4 ~ x…     0.03      0.01      1.84    0.07     0    
    ## 6 x1    y4    log_m… no cova… all     <NA>  y4 ~ x…     0         0.01      0.66    0.51    -0.01 
    ## # … with 9 more variables: conf.high <dbl>, fit_null.devian… <dbl>, fit_df.null <dbl>, 
    ## #   fit_logLik <dbl>, fit_AIC <dbl>, fit_BIC <dbl>, fit_deviance <dbl>, fit_df.residual <dbl>, 
    ## #   fit_nobs <dbl>

Note: In the technical details of the summary, we also always see how
many cores were used and how long the fitting process has taken.

At the end of our analysis, it makes sense to explicitly close
multisession workers by switching the plan back to sequential.

\
[`plan`](https://future.futureverse.org/reference/plan.html)`(``sequential``)`
