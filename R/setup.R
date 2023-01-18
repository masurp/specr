#' Specifying analytical decisions in a specification setup
#'
#' @description Creates all possible specifications as a combination of
#'    different dependent and independent variables, model types, control
#'    variables, potential subset analyses, as well as potentially other
#'    analytic choices. This function represents the first step in the
#'    analytic framework implemented in the package \code{specr}. The resulting
#'    class \code{specr.setup} then needs to be passed to the core function of
#'    the package called \code{specr()}, which fits the specified models across
#'    all specifications.
#'
#' @param data The data set that should be used for the analysis
#' @param y A vector denoting the dependent variables
#' @param x A vector denoting independent variables
#' @param model A vector denoting the model(s) that should be estimated.
#' @param subsets Specification of potential subsets/groups as list.  There are two ways
#'    in which these can be specified that both start from the assumption that the
#'    "grouping" variable is in the data set. The simplest way is to provide a named
#'    vector within the list, whose name is the variable that should be used for
#'    subsetting and whose values are the values that reflect the subsets (e.g.,
#'    `list(group2 = c("female", "male")`). In this case, the specifications will
#'    includes "all", "only female" and "only male". Alternatively, you can also use
#'    the `unique` function to extract that vector directly from the data set
#'    (e.g., `list(group2 = unique(example_data$group2`). Both approaches lead to the
#'    same result. The former, however, has the advantages that one can also remove some of the
#'    subgroups (e.g. `list(group2 = c("female")`). In this case, the specifications
#'    will include "all" (no subset) and "only females". See examples for more details.
#' @param controls A vector of the control variables that should be included.
#'    Defaults to NULL.
#' @param add_to_formula A string specifying aspects that should always be
#'    included in the formula (e.g. a constant covariate, random effect structures...)
#' @param fun1 A function that extracts the parameters of interest from the
#'    fitted models. Defaults to [tidy][broom::broom-package], which works
#'    with a large range of different models.
#' @param fun2 A function that extracts fit indices of interest from the models.
#'    Defaults to [glance][broom::broom-package], which works with a large range of
#'    different models. Note: Different models result in different fit indices. Thus,
#'    if you use different models within one specification curve analysis, this may not
#'    work. In this case, you can simply set `fun2 = NULL` to not extract any fit indices.
#' @param simplify Logical value indicating what type of combinations between
#'    control variables should be included in the specification. If FALSE (default),
#'    all combinations between the provided variables are created (none, each
#'    individually, each combination between each variable, all variables). If
#'    TRUE, only no covariates, each individually, and all covariates are included
#'    as specifications (akin to the default in specr version 0.2.1).
#'
#' @return An object of class \code{specr.setup} which includes all possible
#'    specifications based on combinations of the analytic choices. The
#'    resulting list includes a specification tibble, the data set, and additional
#'    information about the universe of specifications. Use
#'    \code{methods(class = "specr.setup")} for an overview on available methods.
#'
#' @details Empirical results are often contingent on analytical decisions that
#'    are equally defensible, often arbitrary, and motivated by different reasons.
#'    This decisions may introduce bias or at least variability. To this end,
#'    specification curve analyses  (Simonsohn et al., 2020) or multiverse
#'    analyses (Steegen et al., 2016) refer to identifying the set of
#'    theoretically justified, statistically valid (and potentially also non-redundant
#'    specifications, fitting the "multiverse" of models represented by these
#'    specifications and extract relevant parameters often to display the results
#'    graphically as a so-called specification curve. This allows readers to
#'    identify consequential specifications decisions and how they affect the results
#'    or parameter of interest.
#'
#'    \bold{Use of this function}
#'
#'    A general overview is provided in the vignettes \code{vignette("specr")}.
#'    It is assumed that you want to estimate the relationship between two variables
#'    (\code{x} and \code{y}). What varies may be what variables should be used for
#'    \code{x} and \code{y}, what model should be used to estimate the relationship,
#'    whether the relationship should be estimated for certain subsets, and whether
#'    different combinations of control variables should be included. This
#'    allows to (re)produce almost any analytical decision imaginable. See examples
#'    below for how a number of typical analytical decision can be implemented.
#'    Afterwards you pass the resulting object of a class \code{specr.setup} to the
#'    function \code{specr()} to run the specification curve analysis.
#'
#'    Note, the resulting class of \code{specr.setup} allows to use generic functions.
#'    Use \code{methods(class = "specr.setup")} for an overview on available methods and
#'    e.g., \code{?summary.specr.setup} to view the dedicated help page.
#'
#' @references \itemize{
#'  \item Simonsohn, U., Simmons, J.P. & Nelson, L.D. (2020). Specification curve analysis. *Nature Human Behaviour, 4*, 1208–1214. https://doi.org/10.1038/s41562-020-0912-z
#'  \item Steegen, S., Tuerlinckx, F., Gelman, A., & Vanpaemel, W. (2016). Increasing Transparency Through a Multiverse Analysis. *Perspectives on Psychological Science, 11*(5), 702-712. https://doi.org/10.1177/1745691616658637
#' }
#' @export
#'
#' @seealso [specr()] for the second step of actually running the actual specification curve analysis
#' @seealso [summary.specr.setup()] for how to summarize and inspect the resulting specifications
#' @seealso [plot.specr.setup()] for creating a visual summary of the specification setup.
#'
#' @examples
#' ## Example 1 ----
#' # Setting up typical specifications
#' specs <- setup(data = example_data,
#'    x = c("x1", "x2"),
#'    y = c("y1", "y2"),
#'    model = "lm",
#'    controls = c("c1", "c2", "c3"),
#'    subsets = list(group1 = c("young", "middle", "old"),
#'                   group2 = c("female", "male")),
#'    simplify = TRUE)
#'
#' # Check specifications
#' summary(specs, rows = 18)
#'
#'
#' ## Example 2 ----
#' # Setting up specifications for multilevel models
#' specs <- setup(data = example_data,
#'    x = c("x1", "x2"),
#'    y = c("y1", "y2"),
#'    model = c("lmer"),                                   # multilevel model
#'    subsets = list(group1 = c("young", "old"),           # only young and old!
#'                   group2 = unique(example_data$group2)),# alternative specification
#'    controls = c("c1", "c2"),
#'    add_to_formula = "(1|group2)")                       # random effect in all models
#'
#' # Check specifications
#' summary(specs)
#'
#'
#' ## Example 3 ----
#' # Setting up specifications with a different parameter extract functions
#'
#' # Create custom extract function to extract different parameter and model
#' tidy_99 <- function(x) {
#'   fit <- broom::tidy(x,
#'      conf.int = TRUE,
#'      conf.level = .99)         # different alpha error rate
#'   fit$full_model = list(x)     # include entire model fit object as list
#'   return(fit)
#' }
#'
#' # Setup specs
#' specs <- setup(data = example_data,
#'    x = c("x1", "x2"),
#'    y = c("y1", "y2"),
#'    model = "lm",
#'    fun1 = tidy_99,             # pass new function to setup
#'    add_to_formula = "c1 + c2") # set of covariates in all models
#'
#' # Check specifications
#' summary(specs)
setup <- function(data,
                  x,
                  y,
                  model,
                  controls = NULL,
                  subsets = NULL,
                  add_to_formula = NULL,
                  fun1 = function(x) broom::tidy(x, conf.int = TRUE),
                  fun2 = function(x) broom::glance(x),
                  simplify = FALSE) {

  . <- group_var <- check <- check2 <- NULL

  if (rlang::is_missing(data)) {
    stop("You must provide the data set that should be used in the analyses.")
  }

  if (rlang::is_missing(x)) {
    stop("You must specify at least one independent variable `x`.")
  }

  if (rlang::is_missing(y)) {
    stop("You must specify at least one dependent variable `y`.")
  }

  if (rlang::is_missing(model)) {
    stop("You must name at least one model function (e.g., 'lm').")
  }

  if (any(duplicated(x))| any(duplicated(y)) | any(duplicated(model)) | any(duplicated(controls))) {
    stop("Duplicate values in x, y, model, and controls are not allowed.")
  }

  if (!is.null(subsets) & !inherits(subsets, "list")) {
    stop("Subsets must be an object of class 'list'")
  }

  if(isTRUE(simplify)) {
    controls <- expand_covariate_simple(controls)
  } else {
    controls <- expand_covariate(controls)
  }

  if(!is.null(subsets)) {

    # Create all subset combinations
    group_vars <- subsets %>%
      map(function(x) append(x, NA)) %>%
      expand.grid

    # Exand across choices
    grid <- expand_grid(
      x,
      y,
      model,
      controls,
      group_vars
    )

  } else {

    # Expand across choices
    grid <- expand_grid(
      x,
      y,
      model,
      controls,
      group_var = "all"
    )

}

  # Filter out non-meaningful specs (e.g., where control and independent are the same)
  grid <- grid %>%
    rowwise %>%
    mutate(check = grepl(x, controls)) %>%
    mutate(check2 = grepl(y, controls)) %>%
    filter(isFALSE(check)) %>%
    filter(isFALSE(check2)) %>%
    select(-check, -check2) %>%
    ungroup


  # In case nothing should be addded to the formula
  if(is.null(add_to_formula)) {

    grid <- grid %>%
      mutate(formula = str_glue("{y} ~ {x} + {controls}"))

  # In case something should be added to the formula
  } else {

    grid <- grid %>%
      mutate(formula = str_glue("{y} ~ {x} + {controls} + {add_to_formula}"))
  }

  # Transform model string into actual function that also extracts parameters
  grid <- grid %>%
    mutate(model_function = purrr::map(model,
                                       function(x) tidy_model(x,
                                                              fun1 = fun1,
                                                              fun2 = fun2)))

  pos_formula <- which(names(grid) == "formula")
  subsets_names <- grid[5:pos_formula] %>%
    select(-formula) %>%
    names

  if(subsets_names[1] != "group_var") {

    # Create subset variable (combination of group variables)
     grid <- grid %>%
       mutate(dplyr::across(all_of(subsets_names), ~ as.character(.))) %>%
       tidyr::unite(subsets, all_of(subsets_names), sep = " & ", remove = FALSE) %>%
       mutate(across(all_of(subsets_names),~ as.factor(.)),
              subsets = stringr::str_remove_all(subsets, "NA & "),
              subsets = stringr::str_remove_all(subsets, " & NA"),
              subsets = ifelse(subsets == "NA", "all", subsets))

  } else {

    grid <- grid %>%
      rename(subsets = group_var)
    subsets_names <- "none"
  }

  # Transform controls variable
  grid <- grid %>%
    mutate(controls = ifelse(controls == "1", "no covariates", controls))

  if(isTRUE(simplify)) {

    grid <- grid %>%
      mutate(controls = ifelse(grepl("\\+", controls), "all covariates", controls))

  }

    # Create list of relevant objects
    res <- list(specs = grid,
                n_specs = nrow(grid),
                x = x,
                y = y,
                model = model,
                controls = controls,
                subsets = subsets_names,
                fun1 = fun1,
                fun2 = fun2,
                add_to_formula = add_to_formula,
                data = data)

    # Set class
    class(res) <- "specr.setup"

    return(res)

}


