# Incorporating Bayesian Statistics

This vignette shows how to incorporate Bayesian modeling using the
package [brms](https://paulbuerkner.com/brms/) in specr. When we fit a
model with `brms`, the package actually calls Rstan in the background,
which, in turn, is an R interface to the statistical programming
language `Stan`. Stan is built in the programming language C++ and
models have to be compiled using C++ to be run. This is all taken care
of by brms, so you just need to run `brm(…)` to fit any model. `brms`
uses a syntax for specifying model formulae that is based on the syntax
of the commonly known lme4 package (for multilevel modeling). The
comparatively easy syntax of `brms` is then converted into Stan code
automatically.

That said, since the models have to be compiled in C++, you need to set
up your computer so that it can actually use C++. This has to be done
only once, before installing brms. Unfortunately, as almost always, how
you install C++ depends on your operating system. Check [this
vignette](https://github.com/masurp/workshop_bayes/blob/main/exercises/Exercise_02.md)
to if you have problems installing brms.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`tidyverse`](https://tidyverse.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`specr`](https://masurp.github.io/specr/)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`brms`](https://github.com/paul-buerkner/brms)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`broom.mixed`](https://github.com/bbolker/broom.mixed)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggridges`](https://wilkelab.org/ggridges/)`)`

## Setting up custom functions

Although you can simply pass the function `brm` to `setup` without any
specific custom function, it usually makes sense to set up some custom
model fitting and extraction functions to make sure
[`specr()`](https://masurp.github.io/specr/reference/specr.md) does
exactly what we want.

Here, I am creating a custom model fitting function and specify relevant
parameters for the Bayesian models. Here, I am only switching off the
progress indication and messages, and reduce the iterations to 1000 to
save time. Of course, we can bump this up to wherever we need it to be.

Note that you can use specr to parallelize the process (then you would
set `cores = 1` in the brm function) and use the parallelization via
`furrr` as outlined in [this
vignette](https://masurp.github.io/specr/articles/parallelization.html).
Alternatively, we can let `brms` parallelize the process (e.g., by
setting `cores = 4`). Then, we would not use the parallelization
procedure by `specr`. Here, I keep it simple and just do all
computations sequentially. Thus, all chains per specifications as well
as the specifications themselves are fitted one after the other.

If you want to be able to inspect each brms-object after the
computation, I suggest to also create a custom fit extract function.
Here, I added the full `brms.fit` object to the `glance` output. This
way, we can later access the entire object in order to e.g., extract
posterior distributions or other aspects of the Bayesian models.

\
`# Customized function, not necessary if standard values are to be used`\
`brm_new`` ``<-`` ``function``(``formula``, ``data``)`` ``{`\
`  `[`brm`](https://paulbuerkner.com/brms/reference/brm.html)`(``formula``, ``data``,`\
`      silent ``=`` ``2``,    ``# Don't print progress`\
`      refresh ``=`` ``0``,   ``# Remove message (not necessary, but nicer in the output)`\
`      iter ``=`` ``1000``)``   ``# I just set this here to reduce computing time, can be higher, of course`\
`}`\
\
`# New fun2, fit extract function`\
`glance_brm`` ``<-`` ``function``(``x``)`` ``{`\
`  ``fit2`` ``<-`` ``broom``::`[`glance`](https://generics.r-lib.org/reference/glance.html)`(``x``)`\
`  ``fit2``$``full_model`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(``x``)``  ``# add full model`\
`  `[`return`](https://rdrr.io/r/base/function.html)`(``fit2``)`\
`}`\
\
`# Setting up specifications`\
`specs`` ``<-`` `[`setup`](https://masurp.github.io/specr/reference/setup.md)`(``data ``=`` ``example_data``,`\
`              x ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"x1"``, ``"x2"``, ``"x3"``)``,`\
`              y ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"y1"``, ``"y2"``)``,`\
`              model ``=`` ``"brm_new"``,`\
`              controls ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"c1"``, ``"c2"``)``,`\
`              fun2 ``=`` ``glance_brm``)`\
`# Check specs`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``specs``)`\
`#> Setup for the Specification Curve Analysis`\
`#> -------------------------------------------`\
`#> Class:                      specr.setup -- version: 1.0.0 `\
`#> Number of specifications:   24 `\
`#> `\
`#> Specifications:`\
`#> `\
`#>   Independent variable:     x1, x2, x3 `\
`#>   Dependent variable:       y1, y2 `\
`#>   Models:                   brm_new `\
`#>   Covariates:               no covariates, c1, c2, c1 + c2 `\
`#>   Subsets analyses:         all `\
`#> `\
`#> Function used to extract parameters:`\
`#> `\
`#>   function(x) broom::tidy(x, conf.int = TRUE)`\
`#> <environment: 0x7f9c7371bad0>`\
`#> `\
`#> `\
`#> Head of specifications table (first 6 rows):`\
`#> # A tibble: 6 × 6`\
`#>   x     y     model   controls      subsets formula          `\
`#>   <chr> <chr> <chr>   <chr>         <chr>   <glue>           `\
`#> 1 x1    y1    brm_new no covariates all     y1 ~ x1 + 1      `\
`#> 2 x1    y1    brm_new c1            all     y1 ~ x1 + c1     `\
`#> 3 x1    y1    brm_new c2            all     y1 ~ x1 + c2     `\
`#> 4 x1    y1    brm_new c1 + c2       all     y1 ~ x1 + c1 + c2`\
`#> 5 x1    y2    brm_new no covariates all     y2 ~ x1 + 1      `\
`#> 6 x1    y2    brm_new c1            all     y2 ~ x1 + c1`

## Estimating the models

Because we are estimating 24 Bayesian models, this may take a while (see
above for ways to parallelize the computations).

\
`results`` ``<-`` `[`specr`](https://masurp.github.io/specr/reference/specr.md)`(``specs``)`

Now we can again summarize and plot our results.

\
[`summary`](https://rdrr.io/r/base/summary.html)`(``results``)`\
`#> Results of the specification curve analysis`\
`#> -------------------`\
`#> Technical details:`\
`#> `\
`#>   Class:                          specr.object -- version: 1.0.0 `\
`#>   Cores used:                     1 `\
`#>   Duration of fitting process:    9.25 sec elapsed `\
`#>   Number of specifications:       24 `\
`#> `\
`#> Descriptive summary of the specification curve:`\
`#> `\
`#>  median  mad   min  max   q25  q75`\
`#>    0.16 0.25 -0.34 0.62 -0.07 0.25`\
`#> `\
`#> Descriptive summary of sample sizes: `\
`#> `\
`#>  median  min  max`\
`#>    1000 1000 1000`\
`#> `\
`#> Head of the specification results (first 6 rows): `\
`#> `\
`#> # A tibble: 6 × 18`\
`#>   x     y     model   controls      subsets formula      effect component group estimate std.error `\
`#>   <chr> <chr> <chr>   <chr>         <chr>   <glue>       <chr>  <chr>     <chr>    <dbl>     <dbl> `\
`#> 1 x1    y1    brm_new no covariates all     y1 ~ x1 + 1  fixed  cond      <NA>      0.62      0.04 `\
`#> 2 x1    y1    brm_new c1            all     y1 ~ x1 + c1 fixed  cond      <NA>      0.6       0.04 `\
`#> 3 x1    y1    brm_new c2            all     y1 ~ x1 + c2 fixed  cond      <NA>      0.61      0.04 `\
`#> 4 x1    y1    brm_new c1 + c2       all     y1 ~ x1 + c… fixed  cond      <NA>      0.59      0.04 `\
`#> 5 x1    y2    brm_new no covariates all     y2 ~ x1 + 1  fixed  cond      <NA>     -0.33      0.04 `\
`#> 6 x1    y2    brm_new c1            all     y2 ~ x1 + c1 fixed  cond      <NA>     -0.34      0.04 `\
`#> # … with 7 more variable: conf.low <dbl>, conf.high <dbl>, fit_algorithm <chr>, fit_pss <dbl>`\
`#> #   fit_nobs <dbl>, fit_sigma <dbl>, fit_full_model <list>`\
[`plot`](https://rdrr.io/r/graphics/plot.default.html)`(``results``, choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"x"``, ``"y"``, ``"controls"``)``)`

![plot of chunk
unnamed-chunk-4](parallel-bayesian-models-unnamed-chunk-4-1.png "plot of chunk unnamed-chunk-4")

Again, the coefficients are median estimates from the posterior and
their respective 95% HDIs.

## Inspecting specific models

Because we kept the entire brms models in our result data set, we can
explore specific models using the `pull` function.

\
`# Pull entire models from the listed vector`\
`models`` ``<-`` ``results`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``as_tibble`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`pull`](https://dplyr.tidyverse.org/reference/pull.html)`(``fit_full_model``)`\
\
`# Summarize e.g., the first model`\
[`summary`](https://rdrr.io/r/base/summary.html)`(``models``[[``1``]``]``)`\
`#>  Family: gaussian `\
`#>   Links: mu = identity; sigma = identity `\
`#> Formula: y1 ~ x1 + 1 `\
`#>    Data: data (Number of observations: 1000) `\
`#>   Draws: 4 chains, each with iter = 1000; warmup = 500; thin = 1;`\
`#>          total post-warmup draws = 2000`\
`#> `\
`#> Population-Level Effects: `\
`#>           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS`\
`#> Intercept    -1.10      0.04    -1.17    -1.02 1.00     1915     1356`\
`#> x1            0.62      0.04     0.55     0.69 1.00     1900     1631`\
`#> `\
`#> Family Specific Parameters: `\
`#>       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS`\
`#> sigma     1.14      0.03     1.09     1.18 1.00     2903     1542`\
`#> `\
`#> Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS`\
`#> and Tail_ESS are effective sample size measures, and Rhat is the potential`\
`#> scale reduction factor on split chains (at convergence, Rhat = 1).`

From these model fit objects, we can also extract posterior
distributions for the parameters of interest and stort them in one data
frame.

\
`posteriors`` ``<-`` ``results`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``as_tibble`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`pull`](https://dplyr.tidyverse.org/reference/pull.html)`(``fit_full_model``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`map`](https://purrr.tidyverse.org/reference/map.html)`(``function``(``x``)`` `[`as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)`(`[`as_draws_df`](https://mc-stan.org/posterior/reference/draws_df.html)`(``x``)``)``[``, ``2``]`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`        `[`gather`](https://tidyr.tidyverse.org/reference/gather.html)`(``key``, ``value``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html)`(``.``, .id ``=`` ``"id"``)`\
\
`` # First 10 draws from the first specification for predictor `x2` ``\
[`head`](https://rdrr.io/r/utils/head.html)`(``posteriors``, n ``=`` ``10``)`\
`#> # A tibble: 10 × 3`\
`#>    id    key   value`\
`#>    <chr> <chr> <dbl>`\
`#>  1 1     b_x1  0.642`\
`#>  2 1     b_x1  0.655`\
`#>  3 1     b_x1  0.647`\
`#>  4 1     b_x1  0.650`\
`#>  5 1     b_x1  0.611`\
`#>  6 1     b_x1  0.736`\
`#>  7 1     b_x1  0.565`\
`#>  8 1     b_x1  0.624`\
`#>  9 1     b_x1  0.602`\
`#> 10 1     b_x1  0.611`

## Plotting posterior distributions for all specifications

Using the `ggridges` package, we can plot the posterior distributions of
the different specifications.

\
`posteriors`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``id ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``id``, levels ``=`` ``1``:``24``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``value``, y ``=`` ``id``, fill ``=`` ``key``)``)`` ``+`\
`  `[`geom_density_ridges`](https://wilkelab.org/ggridges/reference/geom_density_ridges.html)`(``)`` ``+`\
`  `[`scale_fill_brewer`](https://ggplot2.tidyverse.org/reference/scale_brewer.html)`(``palette ``=`` ``"Blues"``)`` ``+`\
`  `[`theme_minimal`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``x ``=`` ``"Posterior"``,`\
`       y ``=`` ``"Specifications"``,`\
`       fill ``=`` ``""``)`

![plot of chunk
unnamed-chunk-7](parallel-bayesian-models-unnamed-chunk-7-1.png "plot of chunk unnamed-chunk-7")

Of course, the posterior distributions are not sorted in any way. With a
bit of data wrangling, we can create a specification curve consisting of
posterior distributions.

\
`# First panel`\
`p1`` ``<-`` ``results`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``as_tibble`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``id ``=`` `[`as.character`](https://rdrr.io/r/base/character.html)`(``1``:`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``.``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``estimate``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``specifications ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`as.character`](https://rdrr.io/r/base/character.html)`(``1``:`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``.``)``)``,`\
`                                 levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``:``24``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`left_join`](https://dplyr.tidyverse.org/reference/mutate-joins.html)`(``posteriors``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``value``, y ``=`` ``specifications``)``)`` ``+`\
`  `[`stat_density_ridges`](https://wilkelab.org/ggridges/reference/stat_density_ridges.html)`(``quantile_lines ``=`` ``TRUE``,`\
`                      quantiles ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.025``, ``0.975``)``,`\
`                      alpha ``=`` ``.5``,`\
`                      fill ``=`` ``"lightblue"``,`\
`                      color ``=`` ``"black"``)`` ``+`\
`  `[`coord_flip`](https://ggplot2.tidyverse.org/reference/coord_flip.html)`(``)`` ``+`\
`  `[`theme_minimal`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``x ``=`` ``"Posterior"``, y ``=`` ``""``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(``strip.text ``=`` `[`element_blank`](https://ggplot2.tidyverse.org/reference/element.html)`(``)``,`\
`        axis.line ``=`` `[`element_line`](https://ggplot2.tidyverse.org/reference/element.html)`(``"black"``, size ``=`` ``.5``)``,`\
`        legend.position ``=`` ``"none"``,`\
`        panel.spacing ``=`` `[`unit`](https://rdrr.io/r/grid/unit.html)`(``.75``, ``"lines"``)``,`\
`        axis.text ``=`` `[`element_text`](https://ggplot2.tidyverse.org/reference/element.html)`(``colour ``=`` ``"black"``)``)`\
\
`# Second panel`\
`p2`` ``<-`` ``results`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  ``as_tibble`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``estimate``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``specifications ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(`[`as.character`](https://rdrr.io/r/base/character.html)`(``1``:`[`nrow`](https://rdrr.io/r/base/nrow.html)`(``.``)``)``,`\
`                                 levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1``:``24``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`gather`](https://tidyr.tidyverse.org/reference/gather.html)`(``key``, ``value``, `[`c`](https://rdrr.io/r/base/c.html)`(``"x"``, ``"y"``, ``"controls"``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``key ``=`` `[`factor`](https://rdrr.io/r/base/factor.html)`(``key``, levels ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"x"``, ``"y"``, ``"controls"``)``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``)`` ``+`\
`  `[`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``specifications``,`\
`                 y ``=`` ``value``)``,`\
`             shape ``=`` ``124``,`\
`             size ``=`` ``3.35``)`` ``+`\
`  `[`theme_minimal`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`` ``+`\
`  `[`facet_grid`](https://ggplot2.tidyverse.org/reference/facet_grid.html)`(``.data``$``key``~``1``, scales ``=`` ``"free_y"``, space ``=`` ``"free_y"``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(`\
`    axis.line ``=`` `[`element_line`](https://ggplot2.tidyverse.org/reference/element.html)`(``"black"``, size ``=`` ``.5``)``,`\
`    legend.position ``=`` ``"none"``,`\
`    panel.spacing ``=`` `[`unit`](https://rdrr.io/r/grid/unit.html)`(``.75``, ``"lines"``)``,`\
`    axis.text ``=`` `[`element_text`](https://ggplot2.tidyverse.org/reference/element.html)`(``colour ``=`` ``"black"``)``,`\
`    strip.text.x ``=`` `[`element_blank`](https://ggplot2.tidyverse.org/reference/element.html)`(``)``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``x ``=`` ``""``, y ``=`` ``""``)`\
\
`# Combine`\
[`plot_grid`](https://wilkelab.org/cowplot/reference/plot_grid.html)`(``p1``, ``p2``,`\
`          ncol ``=`` ``1``,`\
`          align ``=`` ``"hv"``,`\
`          axis ``=`` ``"tblr"``,`\
`          rel_heights ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``3``, ``2``)``)`

![plot of chunk
unnamed-chunk-8](parallel-bayesian-models-unnamed-chunk-8-1.png "plot of chunk unnamed-chunk-8")
