#' Inference with specification curve analysis
#'
#' It is possible to generate the distributions for any of these test statistics
#' under-the-null analytically (that is, with statistical formulas), because the
#' specifications are neither statistically independent nor part of a single model.
#' This function hence generates such distributions by relying on resampling under-the-null.
#' This involves modifying the observed data so that the null hypothesis is known to be true,
#' and then drawing random samples of the modified data. The resulting object of class
#' `specr.boot` can be summarized and plotted with generic functions.
#'
#' @param x A fitted `specr.object`, resulting from running `specr()`.
#' @param y A `specr.setup` object, resulting from running `setup()`
#' @param n_samples Number of bootstrap samples to be drawn. Defaults to 500.
#' @param ... Further arguments that can be passed to \code{future_map}. This only becomes
#'    important if parallelization is used. When a plan for parallelization is set, one
#'    can also set `.progress = TRUE` to print a progress bar during the fitting process.
#'    See details for more information on parallelization.
#'
#' @keywords internal
#' @return a object of class `specr.boot` that represents a list with the original and bootstrapped results.
#'
#' @details A third step of specification curve analysis involves statistical inference,
#'    answering the question: considering the full set of reasonable specifications jointly,
#'    how inconsistent are the results with the null hypothesis of no effect?
#'
#'     \bold{Parallelization}
#'
#'    By default, the function fits the bootstrapped models across all specifications sequentially
#'    (one after the other). If the data set is large, the models complex (e.g.,
#'    large structural equation models, negative binomial models, or Bayesian models),
#'    and the number of specifications is large, it can make sense to parallelize
#'    these operations. One simply has to load the package `furrr` (which
#'    in turn, builds on `future`) up front. Then parallelizing the fitting process
#'    works as specified in the package description of `furr`/`future` by setting a
#'    "plan" before running `boot_null` such as:
#'
#'    `plan(multisession, workers = 4)`
#'
#'    However, there are many more ways to specifically set up the plan, including
#'    different strategy than `multisession`. For more information, see
#'    `vignette("parallelization")` and the
#'    [reference page](https://future.futureverse.org/reference/plan.html)
#'    for `plan()`.
#'
#' @references \itemize{
#'  \item Simonsohn, U., Simmons, J.P. & Nelson, L.D. (2020). Specification curve analysis. *Nature Human Behaviour, 4*, 1208–1214. https://doi.org/10.1038/s41562-020-0912-z
#' }
#' @export
#'
#' @examples
#' # Setup up  specifications
#'
#' # Requires to keep full model
#' tidy_full <- function(x) {
#' fit <- broom::tidy(x, conf.int = TRUE)
#' fit$res <- list(x)  # Store model object
#' return(fit)
#' }
#'
#' specs <- setup(data = example_data,
#'    y = c("y1", "y2"),
#'    x = c("x1", "x2"),
#'    model = "lm",
#'    controls = c("c1", "c2"),
#'    fun1 = tidy_full)
#'
#' # Run analysis
#' results <- specr(specs)
#'
#' # Run bootstrapping
#' boot_models <- boot_null(results, specs, n_samples = 2) # better 1,000!
#'
#' # Summarize findings
#' summary(boot_models)
#'
#' # Plot under-the-null curves on top of specification curve
#' plot(boot_models)
#'
boot_null <- function(x, y, n_samples = 500, ...) {

  start <- Sys.time()

  controls <- y$controls[!grepl('\\+|^1$', y$controls)]

  # Create samples under-the-null
  null_data <- x %>%
    as_tibble %>%
    tibble::rownames_to_column("number") %>%
    group_by(number) %>%
    mutate(data = list(res[[1]][["model"]])) %>%
    select(-res) %>%
    unnest(cols = c(data)) %>%
    mutate(obs_number = row_number()) %>%
    ungroup %>%
    tidyr::gather(dv, y_value, !!(y$y)) %>%
    tidyr::gather(iv, iv_value, y$x) %>%
    filter(!is.na(iv_value)) %>%
    mutate(y_null = y_value - (estimate * iv_value)) %>%
    select(-y_value, -estimate) %>%
    tidyr::spread(dv, y_null) %>%
    tidyr::spread(iv, iv_value) %>%
    select(-c(std.error, statistic, p.value, conf.low, conf.high, obs_number))

  # Bootstrapping
  boots <- rsample::bootstraps(null_data, times = n_samples, apparent = FALSE)

  # function to fit sca on bootstrapped null data
  fit_sca <- function(split){

    # prep bootstrapped sample
    boot_data <- rsample::analysis(split)

    # run sca
    fit <- specr(
      setup(data = boot_data,
            x = y$x,
            y = y$y,
            controls = controls,
            model = y$model)
    ) %>%
      as_tibble

  }

  if(methods::is(plan(), "sequential")) {

  # run function on each bootstrapped sample in boots
  boot_models = boots %>%
    dplyr::mutate(fit = map(splits, fit_sca, ...)) %>%
    select(-splits)


  } else {

    boot_models = boots %>%
      dplyr::mutate(fit = future_map(splits, fit_sca, ...)) %>%
      select(-splits)

  }

  # Compute time
  end <- Sys.time()
  time <- end-start
  time <- paste(round(as.numeric(time), 3), "sec elapsed")

  # create list
  boot_result <- list(observed_model = as_tibble(x) %>%select(-res),
                      boot_models = boot_models,
                      n_samples = nrow(boot_models),
                      workers = nbrOfWorkers(),
                      time = time)

  # Name class
  class(boot_result) <- "specr.boot"

  return(boot_result)
}
