#' Set up specifications
#'
#' This function creates a tibble that includes all possible specifications based the dependent and independent variables, model types, and control variables that are specified. This function simply produces a tibble of all combinations. It can be used to check the specified analytical choices. This function is called within [run_specs()], which estimates all specified models based on the data that are provided.
#'
#' @param data the data set that should be used for the analysis
#' @param y a vector denoting the dependent variables
#' @param x a vector denoting independent variables
#' @param model a vector denoting the model(s) that should be estimated.
#' @param ... specification of potental subsets/groups. As the subsetting variable should be in the data set, please specifiy as follows (`distinct(data, variable)`). See also examples further below.
#' @param controls a vector of the control variables that should be included. Defaults to NULL.
#' @param add_to_formula a string specifying aspects that should always be inluded in the formula (e.g. a constant covariate, random effect structures...)
#' @param tidy_f A function that extracts the parameters of interest from the fitted models. Defaults to [tidy][broom::broom-package], which works with a large range of models.
#' @param to_tibble If set to TRUE returns a simple data frame (tibble) instead of the S3 object `specr.setup`.
#'
#' @return a `specr.setup` object that includes all possible specifications based on combinations of the analytical choices.
#' @export
#'
#' @seealso [specr] to run the actual specification curve analysis

#' @examples
#' # Example 1 ----
#' # Setting up typical specifications
#' specs <- setup(data = example_data,
#'                x = c("x1", "x2"),
#'                y = "y2",
#'                model = "lm",
#'                controls = c("c1", "c2"),
#'                distinct(example_data, group1))
#'
#' # Check specifications
#' summary(specs)
#'
#' # Example 2 ----
#' # Setting up more advanced specifications
#' specs <- setup(data = example_data,
#'                x = c("x1", "x2"),
#'                y = c("y1", "y2"),
#'                model = c("lmer"),              # multilevel model
#'                distinct(example_data, group1),
#'                controls = c("c1", "c2"),
#'                add_to_formula = "(1|group2)")  # random effect
#'
#' # Check specifications
#' summary(specs, print.specs = TRUE)
#'
#'@seealso [specr()] to run the specification curve analysis.
setup <- function(data,
                  x,
                  y,
                  model,
                  ...,
                  controls = NULL,
                  add_to_formula = NULL,
                  tidy_f = function(x) broom::tidy(x, conf.int = TRUE),
                  to_tibble = FALSE) {

  # Create all subset combinations
  group_vars <- c(...) %>%
    map(function(x) append(x, NA)) %>%
    expand.grid

  # In case there are no subsets:
  if(length(group_vars) == 0) {

    grid <- expand_grid(
      x,
      y,
      model,
      controls = expand_covariate(controls)
    )

  # In case there are subsets
  } else {

    grid <- expand_grid(
      x,
      y,
      model,
      controls = expand_covariate(controls),
      group_vars
    )

  }

  # In case nothing should be addded to the formula
  if(is.null(add_to_formula)) {

    grid <- grid %>%
      dplyr::mutate(formula = str_glue("{y} ~ {x} + {controls}"))

  # In case something should be added to the formula
  } else {

    grid <- grid %>%
      dplyr::mutate(formula = str_glue("{y} ~ {x} + {controls} + {add_to_formula}"))
  }

  # Transform model string into actual function that also extracts parameters
  grid <- grid %>%
    dplyr::mutate(model_function = map(
      model, function(x) tidy_model(x, tidy_f = tidy_f)
    )
    )

  # Should a tibble be returend?
  if(isTRUE(to_tibble)) {

    return(grid)

  # Setup S3 class
  } else {

    # Get subset names
    subsets <- names(group_vars)

    # Create list of relevant objects
    res <- list(specs = grid,
                n_specs = nrow(grid),
                x = x,
                y = y,
                model = model,
                controls = controls,
                subsets = subsets,
                tidy_f = tidy_f,
                add_to_formula = add_to_formula,
                n_specs = nrow(grid),
                data = data)

    # Set class
    class(res) <- "specr.setup"

    return(res)
  }
}

