#' Estimate all specifications
#'
#' This is the central function of the package. It
#'
#'
#' It takes the data frame and vectors for analytical choices related to the dependent variable, the independent variable, the type of models that should be estimated,the set of covariates that should be included (none, each individually, and all together),
#' as well as a named list of potential subsets.
#'
#' @param specs a `specr.setup` object resulting from `setup()` or a tibble that contains the relevant specifications (e.g., a tibble resulting from `setup()`)
#' @param data a data frame or tibble that contains the data set. Does not have to be specified if specs are a `specr.setup` object.
#' @param workers A numeric value that indicates how many cores should be used. Defaults to `availableCores()` from the future package.
#'
#' @return a `specr` object that includes all specifications and the results.
#'
#' @references \itemize{
#'  \item Simonsohn, U., Simmons, J. P., & Nelson, L. D. (2019). Specification Curve: Descriptive and Inferential Statistics for all Plausible Specifications. Available at: https://doi.org/10.2139/ssrn.2694998
#'  \item Steegen, S., Tuerlinckx, F., Gelman, A., & Vanpaemel, W. (2016). Increasing Transparency Through a Multiverse Analysis. Perspectives on Psychological Science, 11(5), 702-712. https://doi.org/10.1177/1745691616658637
#' }
#' @export
#'
#' @examples
#' # Example 1 ----
#' # Setup up specifications (returns "specr.setup" object)
#' specs <- setup(data = example_data,
#'                y = c("y1", "y2"),
#'                x = c("x1", "x2"),
#'                model = "lm",
#'                distinct(example_data, group1),
#'                controls = c("c1", "c2"))
#'
#' # Run analysis (returns "specr" object)
#' results <- specr(specs,
#'                  workers = 1)   # Not parallelized, using simple `map` functions
#'
#' # Summary of the "specr" object
#' summary(results)
#' summary(results, what = "curve", subsets)
#'
#' # Example 2 ----
#' # Setup up specifications, but return tibble (if you don't want to work with S3 classes)
#' specs2 <- setup(data = example_data,
#'                 y = c("y1", "y2"),
#'                 x = c("x1", "x2"),
#'                 model = "lm",
#'                 distinct(example_data, group1),
#'                 distinct(example_data, group2),
#'                 controls = "c1",
#'                 add_to_formula = "c2",
#'                 to_tibble = TRUE)               # set to TRUE to return tibble
#'
#' # Run analysis, but add data as "specs" is only a tibble of the specifications and doesn't include the data
#' results2 <- specr(specs2,
#'                   data = example_data, # provide data
#'                   workers = 4)         # using 4 cores and `future_map` functions
#'
#' # Check results (returned as a tibble in this case)
#' summary(results2)
#'
#' # Get tibble from "specr" object
#' as_tibble(results2)
specr <- function(specs,
                  data = NULL,
                  workers = availableCores(),
                  message = TRUE){

  # Start timing
  tictoc::tic()

  # Collect data and subsets
  if(class(specs)[1] != "specr.setup") {

    specs <- specs

  } else {

    data <- specs$data
    subsets <- specs$subsets
    specs <- specs$specs

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
      }),
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

  if(isTRUE(message)) {

  # Print short summary
  cat("specr -- version:", as.character(packageVersion("specr")), "\n")
  cat("-------------------\n")
  cat("Models fitted across", nrow(res), "specifications\n")
  cat("Cores used:", workers, "\n")
  time <- tictoc::toc()

  } else {

  time <- tictoc::toc(quiet = TRUE)
  }

  pos_formula <- which(names(res) == "formula")
  subsets_names <- res[5:pos_formula] %>%
    dplyr::select(-formula) %>%
    names

  # Create
  res <- res %>%
    dplyr::mutate(dplyr::across(all_of(subsets_names), ~ as.character(.)),
                  dplyr::across(all_of(subsets_names), ~ ifelse(is.na(.), "all", .))) %>%
    dplyr::mutate(controls = ifelse(controls == "1", "no covariates", controls)) %>%
    tidyr::unite(., subsets, all_of(subsets_names), sep = " & ", remove = FALSE) %>%
    dplyr::mutate(subsets = sub(" & all", "", subsets))


  # Create S3 class
  output <- list(data = res,
                 n_specs = nrow(res),
                 subsets = subsets_names,
                 workers = workers,
                 time = time)

  # Set class
  class(output) <- "specr.object"

  return(output)
}
