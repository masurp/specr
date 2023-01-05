#' Fit models across all specifications
#'
#' @description Runs the specification/multiverse analysis across specified models.
#'    This is the central function of the package and represent the second step
#'    in the analytic framework implemented in the package \code{specr}. It estimates
#'    and returns respective parameters and estimates of models that were specified via \code{setup()}.
#'
#' @param specs A `specr.setup` object resulting from \code{setup} or a tibble that
#'    contains the relevant specifications (e.g., a tibble resulting from \code{setup})
#' @param workers A numeric value that indicates how many cores should be used.
#'    Defaults to \code{availableCores()} from the future package and thus uses
#'    all available cores. Note: If the models specified via \code{setup()} are not
#'    very computationally intensive, the overhead introduced trhough the parallelization
#'    (setting up several workers and R sessions, etc.) may actually make the fitting
#'    process slower. It only becomes considerably faster if the model fitting process
#'    is so intensive that the overhead no longer matters. In other words, in a lot
#'    of cases, setting \code{workers = 1} can be a meaningful choice.
#' @param ... Further arguments that can be passed to \code{future_pmap}. This only becomes
#'    important if parallelization is used (workers > 1) and a custom model function
#'    is used. See examples 2 and 3 below for further details.
#' @param message Logical value; indicates whether a short message should be
#'   printed after the estimation including information about the elapsed time.
#'   Defaults to TRUE.
#'
#' @return An object of class \code{specr.object}, which includes a data frame
#'   with all specifications their respective results along with many other useful
#'   information about the model. Parameters are extracted via the function passed
#'   to \code{setup}. By default this is \code{broom::tidy()} and the function
#'   \code{broom::glance()}).Several other aspects and information are included in
#'   the resulting class (e.g., number of specifications, time elapsed, subsets
#'   included in the analyses). Use \code{methods(class = "specr.object")} for
#'   an overview on available methods.
#'
#' @details Empirical results are often contingent on analytical decisions that
#'    are equally defensible, often arbitrary, and motivated by different reasons.
#'    This decisions may introduce bias or at least variability. To this end,
#'    specification curve analyses  (Simonsohn et al., 2019) or multiverse
#'    analyses (Steegen et al., 2016) refer to identifying the set of
#'    theoretically justified, statistically valid (and potentially also non-redundant
#'    specifications, fitting the "multiverse" of models represented by these
#'    specifications and estract relevant parameters often to display the results
#'    graphically as a so-called specification curve. This allows readers to
#'    identify consequential specifications decisions and how they affect the results
#'    or parameter of interest.
#'
#'    \bold{Use of this function}
#'
#'    A general overview is provided in the vignettes \code{vignette("specr")}.
#'    Generally, you create relevant specification using the function \code{setup()}.
#'    You then pass the resulting object of a class \code{specr.setup} to the
#'    present function \code{specr()} to run the specification curve analysis. Further
#'    note that the resulting object of class \code{specr.object} allows to use
#'    several generic function such as \code{summary()} or \code{plot()}. Use
#'    \code{methods(class = "specr.object")} for an overview on available methods and
#'    e.g., \code{?plot.specr.object} to view the dedicated help page.
#'
#'
#'    \bold{Disclaimer}
#'
#'    We do see a lot of value in investigating how analytical choices
#'    affect a statistical outcome of interest. However, we strongly caution
#'    against using specr as a tool to somehow arrive at a better estimate
#'    compared to a single model. Running a specification curve analysis
#'    does not make your findings any more reliable, valid or generalizable
#'    than a single analysis. The method is meant to inform about the effects of
#'    analytical choices on results, and not a better way to estimate a correlation or effect.
#'
#'
#' @references \itemize{
#'  \item Simonsohn, U., Simmons, J. P., & Nelson, L. D. (2019). Specification Curve: Descriptive and Inferential Statistics for all Plausible Specifications. Available at: https://doi.org/10.2139/ssrn.2694998
#'  \item Steegen, S., Tuerlinckx, F., Gelman, A., & Vanpaemel, W. (2016). Increasing Transparency Through a Multiverse Analysis. Perspectives on Psychological Science, 11(5), 702-712. https://doi.org/10.1177/1745691616658637
#' }
#' @export
#'
#' @examples
#' # Example 1 ----
#' # Setup up typical specifications
#' specs <- setup(data = example_data,             # providing data
#'                y = c("y1", "y2"),               # different y variables
#'                x = c("x1", "x2"),               # different x variables
#'                model = "lm",                    # only one model type
#'                distinct(example_data, group1), # subset based on "group1"
#'                controls = c("c1", "c2"))        # Control for two variables
#'
#' # Run analysis (not parallelized, using simple `map` functions)
#' results <- specr(specs, workers = 1)
#'
#' # Summary of the results
#' summary(results)
#' summary(results, type = "curve", subsets)
#' plot(results)  # For more information on how to plot result see \code{?plot.specr.object}
#'
#' # Example 2 ----
#' # Setup up specifications with specific additions to the formula
#' specs2 <- setup(data = example_data,
#'                 y = c("y1", "y2"),
#'                 x = c("x1", "x2"),
#'                 model = "lm",
#'                 distinct(example_data, group1),
#'                 distinct(example_data, group2),
#'                 controls = "c1",
#'                 add_to_formula = "c2")  # In all models, we now control for "c2"
#'
#' # Run analysis using parallelization (2 cores and `future_pmap`).
#' # Because "lm" is a base function, no further options need to be specified.
#' results2 <- specr(specs2, workers = 2)
#'
#' # Summary of results
#' summary(results2, digits = 3)
#'
#'
#'# Example 3 ----
#' # Create custom function
#' glm2 <- function(formula, data) {
#'   glm(formula, data, family = gaussian())
#' }
#'
#' Setup specifications
#' specs3 <- setup(data = example_data,
#'                 y = c("y1", "y2"),
#'                 x = c("x1", "x2"),
#'                 model = c("lm", "glm2"), # custom function as alternative model
#'                 distinct(example_data, group1),
#'                 distinct(example_data, group2),
#'                 controls = "c1",
#'                 add_to_formula = "c2")
#'
#' # Because "glm2" is a custom function, we need to provide `furrr_options`
#' opts <- furrr_options(globals = list(glm2 = glm2))
#'
#' # Run analysis using parallelization (4 cores and `future_pmap`).
#' results2 <- specr(specs2,
#'                   workers = 4,
#'                   .options = opts) # Pass opts to `specr`, which passes it to `future_pmap()`
#'
#' # Summary of results
#' summary(results2, digits = 3)
#' plot(results2)
#'
#' # You can also extract the results tibble from the S3 class with standard functions
#' as_tibble(results2)
#' as.data.frame(results2)
specr <- function(x,
                  workers = availableCores(),
                  ...,
                  message = TRUE){


  # Start timing
  tictoc::tic()

  if (rlang::is_missing(specs)) {
    stop("You need to provide a class 'specr.setup' or a data frame with specifications. Use 'setup()' to create such a specification setup.")
  }

  if(class(specs)[1] == "tbl_df" & rlang::is_missing(data)) {
    stop("You provided a tibble with all the specifications. In that case, you also need to provide the data set that should be used for the analyses.")
  }

  # Collect data and subsets
  if(class(specs)[1] != "specr.setup") {

    specs <- x

  } else {

    data <- x$data
    subsets <- x$subsets
    specs <- x$specs

  }

  # Differentiate between 1 and >1 workers
  if(workers > 1) {

    # Start several works
    plan(multisession, workers = workers)

    res <- specs %>%
      dplyr::mutate(out = future_pmap(., function(...) {

        # Initialize specs as list
        l = list(...)

        # identify the grouping columns
        group_i <- which(sapply(l, is.factor))
        s <- rep(TRUE, nrow(data))

        # Create relevant subsets
        for (i in group_i) {
          column <- names(l)[i]
          value <- l[[i]]
          if (is.na(value)) next
          s <- s & data[[column]] == value
        }

        # Iterate across specifications
        do.call(
          what = l$model_function,
          args = list(
            formula = l$formula,
            data = data[s,])
        )
      },
      ...),
      ) %>%
      tidyr::unnest(out)

  } else {

    res <- specs %>%
      dplyr::mutate(out = pmap(., function(...) {

        # Initialize specs as list
        l = list(...)

        # identify the grouping columns
        group_i <- which(sapply(l, is.factor))
        s <- rep(TRUE, nrow(data))

        # Create relevant subsets
        for (i in group_i) {
          column <- names(l)[i]
          value <- l[[i]]
          if (is.na(value)) next
          s <- s & data[[column]] == value
        }

        # Iterate across specifications
        do.call(
          what = l$model_function,
          args = list(
            formula = l$formula,
            data = data[s,])
        )
      })) %>%
      tidyr::unnest(out)

  }

  # Select relevant term
  if("op" %in% names(res)) {
    res <- res %>%
      dplyr::filter(term == paste(y, "~", x))
  } else {
    res <- res %>%
      dplyr::filter(term == x)
  }

  pos_formula <- which(names(res) == "formula")
  subsets_names <- res[5:pos_formula] %>%
    dplyr::select(-formula) %>%
    names

  if(isTRUE(message)) {

  # Print short summary
  cat("Models fitted based on", nrow(res), "specifications\n")
  cat("Cores used:", workers, "\n")
  time <- tictoc::toc()

  } else {

  time <- tictoc::toc(quiet = TRUE)

  }


  # Create S3 class
  output <- list(data = res,
                 n_specs = nrow(res),
                 x = x$x,
                 y = x$y,
                 model = x$model,
                 controls = x$model,
                 subsets = x$subsets,
                 workers = workers,
                 time = time)

  # Set class
  class(output) <- "specr.object"

  return(output)
}
