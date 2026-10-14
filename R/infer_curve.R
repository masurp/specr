#' Inference on the specification curve
#'
#' @description Tests how inconsistent the specification curve as a whole is
#'    with the null hypothesis that the independent variable has no effect in any
#'    of the specifications. Following Simonsohn, Simmons and Nelson (2020, section
#'    "Inference with non-experimental data"), the function generates the
#'    distribution of specification curves that would be expected under the null
#'    by resampling data in which the null hypothesis is known to be true. The
#'    resulting object of class `specr.infer` can be summarized with
#'    [summary()][summary.specr.infer] (test statistics and p-values) and plotted
#'    with [plot()][plot.specr.infer] (observed curve against the curves under the
#'    null).
#'
#' @param x A `specr.object`, resulting from running [specr()] (or the tibble
#'    that [specr()] returns when it is given a tibble of specifications).
#' @param data The data set that was used to fit the specifications, or the
#'    `specr.setup` object created by [setup()] (whose data set is then used).
#' @param n_samples Number of bootstrap samples drawn under the null. Defaults to
#'    500. Use at least 1,000 for results that are reported.
#' @param fast Logical value indicating whether specifications that were fitted
#'    with `lm` should be refitted with a fast least-squares routine. It gives the
#'    same estimates and p-values as `lm()`, which is checked against the observed
#'    results for every specification before it is used. Set to `FALSE` to refit
#'    all specifications with their original model function. Defaults to `TRUE`.
#' @param ... Further arguments passed to [furrr::future_map()], e.g.
#'    `.progress = TRUE`. The argument `.options` cannot be set, because the
#'    resampling always uses `furrr_options(seed = TRUE)`.
#'
#' @details \bold{Procedure}
#'
#'    For every specification k, the dependent variable is first modified so that
#'    the null hypothesis is true by construction: \eqn{y^*_k = y_k - b_k x_k},
#'    where \eqn{b_k} is the estimate observed in that specification. Then, in each
#'    of `n_samples` bootstrap samples, N rows are drawn with replacement from the
#'    data (N being the number of rows of `data`), and every specification is
#'    refitted on these rows with \eqn{y^*_k} as its dependent variable. The same
#'    rows are used for all specifications, which preserves the dependence between
#'    them. Each specification keeps its own model, control variables, subset, and
#'    additions to the formula, so that the curves under the null are based on
#'    exactly the same specifications as the observed curve.
#'
#'    \bold{Requirements}
#'
#'    Because the null is imposed by subtracting \eqn{b_k x_k} from the dependent
#'    variable, the procedure is only valid for models that are linear in the
#'    dependent variable (e.g., `lm`, `lmer`, or `glm` with a gaussian family and
#'    identity link), for numeric independent and dependent variables, and for
#'    unstandardized estimates. Before resampling, `infer_curve()` refits every
#'    specification on the full data with \eqn{y^*_k} and stops if the estimate is
#'    not zero, which catches models and extraction functions for which this does
#'    not hold (e.g., logistic regression). Structural equation models (`lavaan`)
#'    are not supported. Note that rows are resampled independently, which ignores
#'    clustering (e.g., in multilevel data).
#'
#'    \bold{Interpretation}
#'
#'    Joint inference across all specifications is only meaningful if the
#'    specifications are genuinely equivalent, arbitrary alternatives (Del Giudice &
#'    Gangestad, 2021). See `vignette("inferences")` for a discussion.
#'
#'    \bold{Parallelization}
#'
#'    The bootstrap samples are distributed across workers if a parallel plan has
#'    been set up front, e.g. `future::plan(future::multisession, workers = 4)`.
#'    Results are reproducible with [set.seed()] and identical whether they are
#'    computed sequentially or in parallel. For more information, see
#'    `vignette("parallelization")`.
#'
#' @return An object of class `specr.infer`, a list that includes the observed
#'    curve (`observed`), the estimates and p-values under the null (`estimates`
#'    and `p_values`; matrices with one row per bootstrap sample and one column per
#'    specification), and technical information. Use
#'    \code{methods(class = "specr.infer")} for an overview on available methods.
#'
#' @references \itemize{
#'  \item Simonsohn, U., Simmons, J.P. & Nelson, L.D. (2020). Specification curve analysis. *Nature Human Behaviour, 4*, 1208–1214. https://doi.org/10.1038/s41562-020-0912-z
#'  \item Del Giudice, M., & Gangestad, S. W. (2021). A traveler’s guide to the multiverse: Promises, pitfalls, and a framework for the evaluation of analytic decisions. *Advances in Methods and Practices in Psychological Science, 4*(1). https://doi.org/10.1177/2515245920954925
#' }
#'
#' @seealso [summary.specr.infer()] and [plot.specr.infer()] to investigate the results.
#'
#' @export
#'
#' @examples
#' # Setup and run specification curve analysis
#' specs <- setup(data = example_data,
#'    y = "y1",
#'    x = c("x1", "x2"),
#'    model = "lm",
#'    controls = c("c1", "c2"),
#'    subsets = list(group2 = c("female", "male")))
#'
#' results <- specr(specs)
#'
#' # Resample under the null (better 1,000 or more)
#' set.seed(42)
#' inference <- infer_curve(results, specs, n_samples = 200)
#'
#' # Test statistics and p-values
#' summary(inference)
#'
#' # Observed curve against the curves under the null
#' plot(inference)
#'
#' # For comparison: the same analysis after shuffling y1, which removes any
#' # effect of x1 and x2 in all specifications
#' shuffled <- example_data
#' shuffled$y1 <- sample(shuffled$y1)
#' specs_null <- setup(data = shuffled,
#'    y = "y1",
#'    x = c("x1", "x2"),
#'    model = "lm",
#'    controls = c("c1", "c2"),
#'    subsets = list(group2 = c("female", "male")))
#'
#' summary(infer_curve(specr(specs_null), specs_null, n_samples = 200))
infer_curve <- function(x,
                        data,
                        n_samples = 500,
                        fast = TRUE,
                        ...) {

  start <- Sys.time()

  if (inherits(x, "specr.object")) {
    results <- x$data
  } else if (inherits(x, "data.frame")) {
    results <- x
  } else {
    stop("`x` must be the result of `specr()`.")
  }

  if (rlang::is_missing(data)) {
    stop("You must provide the data set used in the analysis (or the `specr.setup` object created by `setup()`).")
  }

  if (inherits(data, "specr.setup")) {
    data <- data$data
  }

  if (!inherits(data, "data.frame")) {
    stop("`data` must be a data frame or the `specr.setup` object created by `setup()`.")
  }

  if (!is.numeric(n_samples) || length(n_samples) != 1 || is.na(n_samples) || n_samples < 1) {
    stop("`n_samples` must be a single positive number.")
  }

  if (".options" %in% names(list(...))) {
    stop("`.options` cannot be set: `infer_curve()` always resamples with `furrr_options(seed = TRUE)`.")
  }

  n_samples <- as.integer(n_samples)
  prep <- prepare_null_specs(results, as.data.frame(data), fast = isTRUE(fast))

  worker <- make_null_worker(n = nrow(data),
                             specs = prep$specs,
                             designs = prep$designs,
                             masks = prep$masks,
                             data = prep$data,
                             fitters = prep$fitters,
                             ols = ols_coef_p,
                             refit = refit_spec)

  # Each bootstrap sample gets its own random number stream, so that results are
  # reproducible with set.seed() and independent of the plan and number of workers
  draws <- furrr::future_map(seq_len(n_samples), worker,
                             .options = furrr::furrr_options(seed = TRUE),
                             ...)

  estimates <- do.call(rbind, lapply(draws, `[[`, "estimate"))
  p_values <- do.call(rbind, lapply(draws, `[[`, "p.value"))

  n_failed <- sum(is.na(estimates))
  if (n_failed > 0) {
    warning(n_failed, " of ", length(estimates), " refits under the null failed and are ignored.")
  }

  time <- difftime(Sys.time(), start, units = "secs")

  res <- list(observed = prep$observed,
              estimates = estimates,
              p_values = p_values,
              n_samples = n_samples,
              n_specs = ncol(estimates),
              n_fast = sum(vapply(prep$specs, `[[`, logical(1), "fast")),
              n_failed = n_failed,
              workers = nbrOfWorkers(),
              time = paste(round(as.numeric(time), 3), "sec elapsed"))

  class(res) <- "specr.infer"

  return(res)

}


# Prepares everything that stays the same across bootstrap samples: the null
# outcome of each specification, its subset of rows, and (for lm) its design matrix
prepare_null_specs <- function(results, data, fast = TRUE) {

  required <- c("x", "y", "model", "formula", "model_function", "estimate")
  missing_cols <- setdiff(required, names(results))

  if (length(missing_cols) > 0) {
    stop("`x` lacks the column(s) ", paste(missing_cols, collapse = ", "),
         ". Please provide the result of `specr()`.")
  }

  if ("op" %in% names(results)) {
    stop("`infer_curve()` does not support structural equation models (lavaan).")
  }

  if (anyDuplicated(as.data.frame(results[intersect(c("formula", "model", "subsets"), names(results))])) > 0) {
    stop("`x` contains more than one estimate per specification. `infer_curve()` needs exactly one row per specification.")
  }

  for (v in unique(c(results$x, results$y))) {
    if (!v %in% names(data)) {
      stop("`", v, "` is not a variable in `data`. `infer_curve()` requires that x and y are names of variables in the data set.")
    }
    if (!is.numeric(data[[v]])) {
      stop("`", v, "` must be numeric, because the null is imposed by subtracting estimate * x from y.")
    }
  }

  n_specs <- nrow(results)
  formulas <- as.character(results$formula)
  p_obs <- if ("p.value" %in% names(results)) results$p.value else rep(NA_real_, n_specs)
  se_obs <- if ("std.error" %in% names(results)) results$std.error else rep(NA_real_, n_specs)

  # Subset columns are factors (see setup()) that refer to variables in the data
  sub_cols <- names(results)[vapply(results, is.factor, logical(1)) & names(results) %in% names(data)]

  # Keep only the variables that are needed to refit the models
  vars <- unlist(lapply(formulas, function(f) all.vars(stats::as.formula(f))))
  data <- data[intersect(unique(c(vars, sub_cols)), names(data))]

  # One logical mask of rows per subset
  masks <- list()
  mask_id <- character(n_specs)
  for (k in seq_len(n_specs)) {
    key <- paste(c("all", vapply(sub_cols, function(col) as.character(results[[col]][k]), character(1))),
                 collapse = "\r")
    if (is.null(masks[[key]])) {
      masks[[key]] <- spec_subset_mask(results[k, ], data, sub_cols)
    }
    mask_id[k] <- key
  }

  # One lean fitting function per model (fits the model and extracts the
  # parameters with the extraction function used for the observed results)
  fitters <- list()
  is_lm <- list()
  for (m in unique(results$model)) {
    model_function <- results$model_function[[match(m, results$model)]]
    fitters[[m]] <- lean_model_function(model_function)
    # lm given by name ("lm") or as function (e.g., model = list(ols = lm))
    f <- environment(model_function)$f
    is_lm[[m]] <- identical(f, "lm") || identical(f, stats::lm)
  }

  specs <- vector("list", n_specs)
  designs <- list()
  design_keys <- character()

  for (k in seq_len(n_specs)) {

    spec <- NULL

    if (fast && is_lm[[results$model[k]]]) {
      ols <- prepare_ols_spec(formulas[k], results$x[k], results$estimate[k], p_obs[k],
                              data, masks[[mask_id[k]]])
      if (!is.null(ols)) {
        # Specifications with the same predictors and rows share one design matrix
        key <- paste(sub("^[^~]*~", "", formulas[k]), mask_id[k])
        id <- NULL
        for (candidate in which(design_keys == key)) {
          if (identical(designs[[candidate]]$rows, ols$design$rows)) {
            id <- candidate
            break
          }
        }
        if (is.null(id)) {
          designs[[length(designs) + 1]] <- ols$design
          design_keys <- c(design_keys, key)
          id <- length(designs)
        }
        spec <- list(fast = TRUE, design = id, j = ols$j, ystar = ols$ystar)
      }
    }

    if (is.null(spec)) {
      spec <- list(fast = FALSE,
                   mask = mask_id[k],
                   x = results$x[k],
                   y = results$y[k],
                   b = results$estimate[k],
                   formula = formulas[k],
                   fitter = results$model[k])
    }

    specs[[k]] <- spec
  }

  designs <- lapply(designs, function(d) d[c("X", "pos")])

  # Check that refitting on the null outcome gives an estimate of zero
  bad <- character()
  for (k in which(!vapply(specs, `[[`, logical(1), "fast"))) {
    s <- specs[[k]]
    d <- data[masks[[s$mask]], , drop = FALSE]
    d[[s$y]] <- d[[s$y]] - s$b * d[[s$x]]
    est0 <- tryCatch(suppressWarnings(refit_spec(fitters[[s$fitter]], s$formula, d, s$x))[1],
                     error = function(e) NA_real_)
    tol <- if (is.finite(se_obs[k])) 1e-3 * se_obs[k] else 1e-6 * max(1, abs(s$b))
    if (!is.finite(est0) || abs(est0) > tol) bad <- c(bad, s$formula)
  }

  if (length(bad) > 0) {
    stop("The null hypothesis could not be imposed for ", length(bad), " specification(s) (e.g., '",
         bad[1], "'): refitting with y - estimate * x did not yield an estimate of zero. ",
         "`infer_curve()` requires models that are linear in the dependent variable ",
         "(e.g., lm, lmer, or glm with a gaussian family) and unstandardized estimates.")
  }

  keys <- intersect(c("x", "y", "model", "controls", "subsets", sub_cols), names(results))
  observed <- tibble::as_tibble(as.data.frame(results[keys]))
  observed$estimate <- results$estimate
  observed$std.error <- se_obs
  observed$p.value <- p_obs

  list(specs = specs,
       designs = designs,
       masks = masks,
       data = data,
       fitters = fitters,
       observed = observed)
}


# Design matrix and null outcome for the fast least-squares route. Returns NULL
# if the fast route does not reproduce the observed estimate and p-value
prepare_ols_spec <- function(formula, x, b, p_obs, data, keep) {

  mf <- tryCatch(stats::model.frame(stats::as.formula(formula),
                                    data = data[keep, , drop = FALSE],
                                    na.action = stats::na.omit),
                 error = function(e) NULL)
  if (is.null(mf)) return(NULL)

  X <- stats::model.matrix(attr(mf, "terms"), mf)
  y <- stats::model.response(mf)
  j <- match(x, colnames(X))
  if (is.na(j) || !is.numeric(y) || is.matrix(y)) return(NULL)

  obs <- ols_coef_p(X, y, j)
  if (!isTRUE(all.equal(obs[1], b, tolerance = 1e-6))) return(NULL)
  if (!is.na(p_obs) && !isTRUE(all.equal(obs[2], p_obs, tolerance = 1e-6))) return(NULL)

  rows <- which(keep)
  omitted <- stats::na.action(mf)
  if (!is.null(omitted)) rows <- rows[-as.integer(omitted)]

  pos <- rep(NA_integer_, nrow(data))
  pos[rows] <- seq_along(rows)

  list(design = list(X = X, pos = pos, rows = rows),
       j = j,
       ystar = y - b * X[, j])
}


# Estimate and p-value of column j in a least-squares fit (same as lm() + summary())
ols_coef_p <- function(X, y, j) {

  fit <- stats::.lm.fit(X, y)
  p <- ncol(X)
  df_residual <- nrow(X) - p
  if (fit$rank < p || df_residual < 1) return(c(NA_real_, NA_real_))

  estimate <- fit$coefficients[j]
  xtx_inv <- chol2inv(fit$qr[seq_len(p), seq_len(p), drop = FALSE])
  se <- sqrt(sum(fit$residuals^2) / df_residual * xtx_inv[j, j])

  c(estimate, 2 * stats::pt(-abs(estimate / se), df_residual))
}


# Fits a model with a lean fitting function and returns estimate and p-value of x
refit_spec <- function(fitter, formula, data, x) {

  tt <- fitter(formula, data)
  tt <- tt[!is.na(tt$term) & tt$term == x, , drop = FALSE]
  if (nrow(tt) != 1) return(c(NA_real_, NA_real_))

  c(tt$estimate, if ("p.value" %in% names(tt)) tt$p.value else NA_real_)
}


# Recovers model and parameter extraction function from a specification's
# model_function (see tidy_model()), skipping the extraction of fit indices
lean_model_function <- function(model_function) {

  env <- environment(model_function)
  f <- env$f
  fun1 <- env$fun1

  if (is.null(f) || !is.function(fun1)) {
    stop("The model function of the specifications could not be recovered. Please provide the result of `specr()`.")
  }

  function(formula, data) fun1(do.call(f, list(formula = formula, data = data)))
}


# Builds the function that refits all specifications on one bootstrap sample.
# Everything it needs is passed in, so that parallel workers receive a small,
# self-contained function
make_null_worker <- function(n, specs, designs, masks, data, fitters, ols, refit) {

  force(n); force(specs); force(designs); force(masks); force(data)
  force(fitters); force(ols); force(refit)

  function(i) {

    idx <- sample.int(n, n, replace = TRUE)
    estimate <- p.value <- rep(NA_real_, length(specs))

    for (k in seq_along(specs)) {

      s <- specs[[k]]

      res <- tryCatch({
        if (s$fast) {
          d <- designs[[s$design]]
          sel <- d$pos[idx]
          sel <- sel[!is.na(sel)]
          ols(d$X[sel, , drop = FALSE], s$ystar[sel], s$j)
        } else {
          d <- data[idx[masks[[s$mask]][idx]], , drop = FALSE]
          d[[s$y]] <- d[[s$y]] - s$b * d[[s$x]]
          refit(fitters[[s$fitter]], s$formula, d, s$x)
        }
      }, error = function(e) c(NA_real_, NA_real_))

      estimate[k] <- res[1]
      p.value[k] <- res[2]
    }

    list(estimate = estimate, p.value = p.value)
  }
}


# Test statistics of Simonsohn et al. (2020) for each row (curve) of the matrices
curve_statistics <- function(est, p, alpha = .05) {

  z <- sign(est) * stats::qnorm(pmax(p, .Machine$double.xmin) / 2, lower.tail = FALSE)

  cbind(median = apply(est, 1, stats::median, na.rm = TRUE),
        n_positive = rowSums(est > 0 & p < alpha, na.rm = TRUE),
        n_negative = rowSums(est < 0 & p < alpha, na.rm = TRUE),
        z = rowSums(z, na.rm = TRUE) / sqrt(rowSums(!is.na(z))))
}


# Share of curves under the null that are at least as extreme as the observed curve
extremity_p <- function(observed, null, direction) {

  null <- null[!is.na(null)]
  if (is.na(observed) || length(null) == 0) return(NA_real_)

  n_extreme <- switch(direction,
                      two.sided = sum(abs(null) >= abs(observed)),
                      greater = sum(null >= observed),
                      less = sum(null <= observed))

  (1 + n_extreme) / (1 + length(null))
}


# Compares observed and null statistics for one set of specifications
curve_inference <- function(est, p, boot_est, boot_p, direction = "two.sided", alpha = .05) {

  obs <- curve_statistics(matrix(est, nrow = 1), matrix(p, nrow = 1), alpha)
  null <- curve_statistics(boot_est, boot_p, alpha)

  side <- switch(direction,
                 greater = "positive",
                 less = "negative",
                 two.sided = if (obs[, "n_positive"] >= obs[, "n_negative"]) "positive" else "negative")

  share_obs <- switch(direction,
                      greater = obs[, "n_positive"],
                      less = obs[, "n_negative"],
                      two.sided = max(obs[, "n_positive"], obs[, "n_negative"]))

  share_null <- switch(direction,
                       greater = null[, "n_positive"],
                       less = null[, "n_negative"],
                       two.sided = pmax(null[, "n_positive"], null[, "n_negative"]))

  quant <- function(v, prob) unname(stats::quantile(v, prob, na.rm = TRUE))

  res <- tibble::tibble(
    statistic = c("median", paste0("share significant (", side, ")"), "aggregated z"),
    estimate = unname(c(obs[, "median"], share_obs, obs[, "z"])),
    null_lower = c(quant(null[, "median"], .025), quant(share_null, .025), quant(null[, "z"], .025)),
    null_upper = c(quant(null[, "median"], .975), quant(share_null, .975), quant(null[, "z"], .975)),
    p.value = c(extremity_p(obs[, "median"], null[, "median"], direction),
                extremity_p(share_obs, share_null, "greater"),
                extremity_p(obs[, "z"], null[, "z"], direction)),
    n_specs = length(est))

  # Without p-values, only the median can be tested
  if (all(is.na(p))) {
    res[2:3, c("estimate", "null_lower", "null_upper", "p.value")] <- NA_real_
  }

  res
}
