#' Summarizing the Specifications Setup
#'
#' \code{summary} method for class "specr.setup". Provides a short summary of the
#'    created specifications (the "multiverse") that lists all analytic choices, prints
#'    the function used to extract the parameters from the model. Finally, if
#'    \code{print.specs = TRUE}, it also shows the head of the actual specification grid.
#'
#' @param object An object of class "specr.setup", usually, a result of a call to `setup`.
#' @param digits The number of digits to use when printing the specification table.
#' @param rows The number of rows of the specification tibble that should be printed.
#' @param print.specs Logical value; if `TRUE`, a head of the specification tibble
#'   is returned and printed.
#' @param ... further arguments passed to or from other methods (currently ignored).
#'
#' @return A printed summary of an object of class \code{specr.setup}.
#'
#' @export
#'
#' @seealso The function [setup()], which creates the "specr.setup" object.
#'
#' @examples
#' # Setup specifications
#' specs <- setup(data = example_data,
#'   x = c("x1", "x2"),
#'   y = c("y1", "y2"),
#'   model = c("lm", "glm"),
#'   controls = c("c1", "c2", "c3"),
#'   subsets = list(group3 = unique(example_data$group3)))
#'
#' # Summarize specifications
#' summary(specs)
summary.specr.setup <- function(object,
                                digits = 2,
                                rows = 6,
                                print.specs = TRUE,
                                ...) {

  cat("Setup for the Specification Curve Analysis\n")
  cat("-------------------------------------------\n")
  cat("Class:                     ", class(object), "-- version:", as.character(utils::packageVersion("specr")), "\n")
  cat("Number of specifications:  ", nrow(object$specs), "\n\n")


  cat("Specifications:\n\n")
  cat("  Independent variable:    ", paste0(unique(object$x), collapse = ", "), "\n")
  cat("  Dependent variable:      ", paste0(unique(object$y), collapse = ", "), "\n")
  cat("  Models:                  ", paste0(unique(object$model), collapse = ", "), "\n")
  cat("  Covariates:              ", paste0(unique(object$specs$controls), collapse = ", "), "\n")
  cat("  Subsets analyses:        ", paste0(unique(object$specs$subsets), collapse = ", "), "\n\n")


  cat("Function used to extract parameters:\n\n")
  cat("  ")
  print(object$fun1)

  if(isTRUE(print.specs)) {
  cat("\n\nHead of specifications table (first", rows, "rows):\n\n")
  object$specs %>%
    select(-"model_function") %>%
    mutate_if(is.numeric, round, digits) %>%
    utils::head(n = rows) %>%
    print
  }

  invisible(object)
}

#' Print method for S3 class "specr.setup"
#' @keywords internal
#' @export
print.specr.setup <- function(x, ...) {

  cat("Number of specifications: ", nrow(x$specs), "\n\n")

  invisible(x)
}

#' Plot visualization of the specification setup
#'
#' @description This function plots a visual summary of the specification setup.
#'   It requires an object of class \code{specr.setup}, usually
#'   the result of calling \code{setup()}.
#'
#' @param x A `specr.setup` object, usually resulting from calling \code{setup()}.
#' @param layout The type of layout to create for the garden of forking path. Defaults to "dendrogram". See `?ggraph` for options.
#' @param circular Should the layout be transformed into a radial representation. Only possible for some layouts. Defaults to FALSE.
#' @param ... further arguments passed to or from other methods (currently ignored).
#'
#' @return A \link[ggplot2]{ggplot} object that can be customized further.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' specs <- setup(data = example_data,
#'    x = c("x1", "x2", "x3"),
#'    y = c("y1", "y2"),
#'    model = c("lm", "glm"),
#'    controls = "c1",
#'    subsets = list(group2 = unique(example_data$group2)))
#'
#' plot(specs)
#' plot(specs, circular = TRUE)
#' }
plot.specr.setup <- function(x,
                             layout = "dendrogram",
                             circular = FALSE,
                             ...) {

  decision <- number <- NULL

  df <- dplyr::tibble(decision = factor(c("model", "x", "y", "controls", "subsets"),
                                 levels = c("model", "x", "y", "controls", "subsets")),
               number = c(length(unique(x$specs$model)),
                          length(unique(x$specs$x)),
                          length(unique(x$specs$y)),
                          length(unique(x$specs$controls)),
                          length(unique(x$specs$subsets))))

  p1 <- df %>%
    ggplot(aes(x = decision, y = number)) +
    geom_col(fill = "steelblue") +
    scale_y_continuous(n.breaks = max(df$number)) +
    theme_classic() +
    labs(x = "", y = "number of options")

  df <- x$specs %>%
    select("model", "x", "y", "controls", "subsets") %>%
    arrange(.data$model, .data$x, .data$y, .data$controls, .data$subsets) %>%
    mutate(start = "raw data", .before = 1) %>%
    mutate(x = paste0(.data$x, " & ", .data$model),
           y = paste0(.data$y, " & ", .data$x),
           controls = paste0(.data$controls, " & ", .data$y),
           subsets = paste0(.data$subsets, " & ", .data$controls))

  # Create edges between successive levels of decisions
  nodes <- c("start", "model", "x", "y", "controls", "subsets")
  edge_labels <- c("model", "independent variable", "dependent variable",
                   "control variables", "subsets")

  edge_list <- purrr::map(seq_along(edge_labels), function(i) {
    df %>%
      select(from = all_of(nodes[i]), to = all_of(nodes[i + 1])) %>%
      distinct() %>%
      mutate(decisions = edge_labels[i])
  }) %>%
    bind_rows()
  # Plot edges
  p2 <- edge_list %>%
    graph_from_data_frame %>%
    ggraph::ggraph(layout = layout,
                   circular = circular) +
    ggraph::geom_edge_diagonal() +
    ggraph::geom_node_point(fill = "white", shape = 21) +
    theme_void()

  # Combine plots
  plot_grid(p1, p2,
            labels = c("A", "B"),
            align = "h",
            ncol = 2)


}


#' Return tibble from specr.setup object
#' @keywords internal
#' @export
as_tibble.specr.setup <- function(x, ...) {
  as_tibble(x$specs, ...)
}

#' Return tibble from specr.setup object
#' @keywords internal
#' @export
as.data.frame.specr.setup <- function(x, ...) {
  as.data.frame(x$specs, ...)
}

#' Summarizing the Specification Curve Analysis
#'
#' `summary` method for class "specr". It provides a printed output including
#'   technical details (e.g., cores used, duration of the fitting process, number
#'   of specifications), a descriptive analysis of the overall specification curve,
#'   a descriptive summary of the resulting sample sizes, and a head of the results.
#'
#' @param object An object of class "specr", usually resulting of a call to `specr`.
#' @param type Different aspects can be summarized and printed. See details for alternative summaries
#' @param group In combination with `what = "curve"`, provide a vector of one or more variables (e.g., subsets, controls,...) that denote the available analytic choices to group summary of the estimate.
#' @param var In combination with `what = "curve"`, unquoted name of parameter to be summarized. Defaults to estimate.
#' @param stats Named vector or named list of summary functions (individually defined summary functions can included). If it is not named, placeholders (e.g., "fn1") will be used as column names.
#' @param digits The number of digits to use when printing the specification table.
#' @param rows The number of rows of the specification tibble that should be printed.
#' @param ... further arguments passed to or from other methods (currently ignored).
#'
#' @return A printed summary of an object of class \code{specr.object}.
#'
#' @export
#'
#' @examples
#' # Setup up specifications (returns object of class "specr.setup")
#' specs <- setup(data = example_data,
#'    y = c("y1", "y2"),
#'    x = c("x1", "x2"),
#'    model = "lm",
#'    controls = c("c1", "c2"),
#'    subsets = list(group1 = unique(example_data$group1)))
#'
#' # Run analysis (returns object of class "specr.object")
#' results <- specr(specs)
#'
#' # Default summary of the "specr.object"
#' summary(results)
#'
#' # Summarize the specification curve descriptively
#' summary(results, type = "curve")
#'
#' # Grouping for certain analytical decisions
#' summary(results,
#'        type = "curve",
#'        group = c("x", "y"))
#'
#' # Using customized functions
#' summary(results,
#'         type = "curve",
#'        group = c("x", "group1"),
#'        stats = list(median = median,
#'                     min = min,
#'                     max = max))
#' @seealso The function used to create the "specr.setup" object: `setup`.
summary.specr.object <- function(object,
                                 type = "default",
                                 group = NULL,
                                 var = .data$estimate,
                                 stats = list(median = median, mad = mad, min = min, max = max,
                                              q25 = function(x) quantile(x, prob = .25),
                                              q75 = function(x) quantile(x, prob = .75)),
                                 digits = 2,
                                 rows = 6,
                                 ...){

  var <- enquo(var)
  type <- match.arg(type, c("default", "curve"))

  # Sample sizes are only available if extracted (see `fun2` in setup())
  has_nobs <- "fit_nobs" %in% names(object$data)

  if(type == "default") {

    # Short technical summary
    cat("Results of the specification curve analysis\n")
    cat("-------------------\n")

    cat("Technical details:\n\n")
    cat("  Class:                         ", class(object), "-- version:", as.character(utils::packageVersion("specr")), "\n")
    cat("  Cores used:                    ", object$workers, "\n")
    cat("  Duration of fitting process:   ", object$time, "\n")
    cat("  Number of specifications:      ", as.numeric(object$n_specs), "\n\n")

    # Short descriptive analysis across all specifications
    cat("Descriptive summary of the specification curve:\n\n")

    object$data %>%
      mutate(.specr_var = !! var) %>%
      summarize_at(".specr_var", stats) %>%
      as.data.frame %>%
      round(digits) %>%
      print(row.names = FALSE)

    cat("\n")

    # Head of the result table
    if (has_nobs) {
    cat("Descriptive summary of sample sizes: \n\n")
    des2 <- object$data %>%
      summarize(median = median(.data$fit_nobs),
                min = min(.data$fit_nobs),
                max = max(.data$fit_nobs)) %>%
      as.data.frame %>%
      round(digits)
    print(des2, row.names = FALSE)

    cat("\n")
    }

    # Head of the result table
    cat("Head of the specification results (first", rows, "rows): \n\n")
    object$data %>%
      select(-"model_function", -"term") %>%
      mutate_if(is.numeric, round, digits) %>%
      utils::head(n = rows) %>%
      print

    return(invisible(object))
  }

  if(type == "curve") {

    if (length(group) == 0) {

      res <- object$data %>%
        mutate(.specr_var = !! var) %>%
        summarize_at(".specr_var", stats)

      if (has_nobs) {
        res <- dplyr::bind_cols(
          res,
          object$data %>%
            dplyr::summarize(obs = median(.data$fit_nobs))
        )
      }

    } else {

      res <- object$data %>%
        dplyr::group_by_at(group) %>%
        mutate(.specr_var = !! var) %>%
        summarize_at(".specr_var", stats) %>%
        ungroup()

      if (has_nobs) {
        res <- dplyr::left_join(
          res,
          object$data %>%
            dplyr::group_by_at(group) %>%
            dplyr::summarize(obs = median(.data$fit_nobs)) %>%
            ungroup(),
          by = group
        )
      }
    }

    return(res)
  }

}

#' Print method for S3 class "specr.object"
#' @keywords internal
#' @export
print.specr.object <- function(x, ...) {

  cat("Models fitted based on", nrow(x$data), "specifications\n")
  cat("Number of cores used:", x$workers, "\n\n")

  # Short descriptive analysis across all specifications
  cat("Descriptive summary of the specification curve:\n\n")

  x$data %>%
    summarize_at("estimate",
                 list(median = median, mad = mad, min = min, max = max,
                      q25 = function(x) quantile(x, prob = .25),
                      q75 = function(x) quantile(x, prob = .75))) %>%
    as.data.frame %>%
    round(2) %>%
    print(row.names = FALSE)

  invisible(x)
}



#' Plot specification curve and analytic choices
#'
#' @description This function plots visualizations of the specification curve
#'   analysis. The function requires an object of class \code{specr.object}, usually
#'   the results of calling \code{specr()} to create a standard visualization of the
#'   specification curve analysis. Several types of visualizations are possible.
#'
#' @param x A `specr.object` object, usually resulting from calling \code{specr()}.
#' @param type What type of figure should be plotted? If \code{type = "default"},
#'   the standard specification curve analysis plot (the specification curve as the
#'   upper panel and an overview of the relevant choices as the lower panel) is
#'   created. If \code{type = "curve"}, only the specification curve (upper panel
#'   of the default plot) is plotted. If \code{type = "choices"}, only the choice
#'   panel (lower part of the default plot) is plotted. If \code{type = "boxplot"},
#'   an alternative visualization of differences between choices is plotted that
#'   summarizes results per choice using box-and-whisker plot(s). If
#'   \code{type = "samplesizes"}, a barplot of sample sizes per specification is
#'   plotted.  See examples for more information.
#' @param var Which parameter should be plotted in the curve? Defaults to
#'    \code{estimate}, but other parameters (e.g., p.value, fit_r.squared,...)
#'    can be plotted too.
#' @param group Should the arrangement of the curve be grouped by a particular choice?
#'    Defaults to NULL, but can be any of the present choices (e.g., x, y, controls...)
#' @param choices A vector specifying which analytic choices should be plotted.
#'     By default, all choices (x, y, model, controls, subsets) are plotted.
#' @param labels Labels for the two parts of the plot
#' @param rel_heights vector indicating the relative heights of the plot.
#' @param desc Logical value indicating whether the curve should the arranged in
#'   a descending order. Defaults to FALSE.
#' @param null Indicate what value represents the 'null' hypothesis (defaults to
#'   zero).
#' @param ci Logical value indicating whether confidence intervals should be
#'   plotted.
#' @param ribbon Logical value indicating whether a ribbon instead should be
#'   plotted
#' @param formula In combination with \code{type = "variance"}, you can provide
#'   a specific formula to extract specific variance components. The syntax of the
#'   formula is based on \code{lme4::lmer()} and thus looks something like, e.g.:
#'   \code{"estimate ~ 1 + (1|x) + (1|y)"} (to estimate the amount of variance
#'   explained by different independent `x` and dependent variables `y`). All other
#'   choices are then subsumed under residual variance. By no formula is provided,
#'   all choices (x, y, model, controls, and subsets) that have more than one alternative
#'   are included. See examples for further details.
#' @param print In combination with \code{type = "variance"}, logical value indicating
#'   whether the intra-class correlations (i.e., percentages of variance explained by
#'   analstical choices) should be printed or not. Defaults to TRUE.
#' @param choice_labels Where should the names of the analytical choices (x, y,
#'   model, ...) be shown in the panel of choices (`type = "default"` or
#'   `"choices"`)? Either `"right"` (default) or `"left"`, next to their options.
#' @param ... further arguments passed to or from other methods (currently ignored).
#'
#' @return A \link[ggplot2]{ggplot} object that can be customized further.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Specification Curve analysis ----
#' # Setup specifications
#' specs <- setup(data = example_data,
#'    y = c("y1", "y2"),
#'    x = c("x1", "x2"),
#'    model = "lm",
#'    controls = c("c1", "c2"),
#'    subsets = list(group1 = unique(example_data$group1),
#'                   group2 = unique(example_data$group2)))
#'
#' # Run analysis
#' results <- specr(specs)
#'
#' # Resulting data frame with estimates
#' as_tibble(results)  # This will be used for plotting
#'
#'
#' # Visualizations ---
#' # Plot results in various ways
#' plot(results)                            # default
#' plot(results, choices = c("x", "y"))     # specific choices
#' plot(results, ci = FALSE, ribbon = TRUE) # exclude CI and add ribbon instead
#' plot(results, type = "curve")
#' plot(results, type = "choices")
#' plot(results, type = "samplesizes")
#' plot(results, type = "boxplot")
#'
#' # Names of the choices on the left, next to their options
#' plot(results, choice_labels = "left")
#'
#'
#' # Grouped plot
#' plot(results, group = controls)
#'
#' # Alternative and specific visualizations ----
#' # Other variables in the resulting data set can be plotted too
#' plot(results,
#'      type = "curve",
#'      var = fit_r.squared,   # extract "r-square" instead of "estimate"
#'      ci = FALSE)
#'
#' # Such a plot can also be extended (e.g., by again adding the estimates with
#' # confidence intervals)
#' library(ggplot2)
#' plot(results, type = "curve", var = fit_r.squared) +
#'   geom_point(aes(y = estimate), shape = 5) +
#'   labs(x = "specifications", y = "r-squared | estimate")
#'
#' # We can also investigate how much variance is explained by each analytical choice
#' plot(results, type = "variance")
#'
#' # By providing a specific formula in `lme4::lmer()`-style, we can extract specific choices
#' # and also include interactions between chocies
#' plot(results,
#'      type = "variance",
#'      formula = "estimate ~ 1 + (1|x) + (1|y) + (1|group1) + (1|x:y)")
#'
#' ## Combining several plots ----
#' # `specr` also exports the function `plot_grid()` from the package `cowplot`, which
#' # can be used to combine plots meaningfully
#' a <- plot(results, "curve")
#' b <- plot(results, "choices", choices = c("x", "y", "controls"))
#' c <- plot(results, "samplesizes")
#' plot_grid(a, b, c,
#'           align = "v",
#'           axis = "rbl",
#'           rel_heights = c(2, 3, 1),
#'           ncol = 1)
#'}
plot.specr.object <- function(x,
                              type = "default",
                              var = .data$estimate,
                              group = NULL,
                              choices = c("x", "y", "model", "controls", "subsets"),
                              labels = c("A", "B"),
                              rel_heights = c(2, 3),
                              desc = FALSE,
                              null = 0,
                              ci = TRUE,
                              ribbon = FALSE,
                              formula = NULL,
                              print = TRUE,
                              choice_labels = c("right", "left"),
                              ...){


  var <- enquo(var)
  group <- enquo(group)
  type <- match.arg(type, c("default", "curve", "choices", "boxplot", "variance", "samplesizes"))
  choice_labels <- match.arg(choice_labels)

  # `group` can also be given as a string (as in summary())
  if (is.character(rlang::quo_get_expr(group))) {
    group <- rlang::quo(!! rlang::sym(rlang::quo_get_expr(group)))
  }

  # Create specification curve plot (only if needed, as it requires confidence intervals)
  if (type %in% c("default", "curve")) {

  plot_a <- x$data %>%
    format_results(var = var, group = group, null = null, desc = desc) %>%
    ggplot(aes(x = .data$specifications,
               y = !! var,
               ymin = .data$conf.low,
               ymax = .data$conf.high,
               color = .data$color)) +
    geom_point(aes(color = .data$color),
               size = 1) +
    theme_minimal() +
    scale_color_identity() +
    theme(strip.text = element_blank(),
          axis.line = element_line("black", linewidth = .5),
          legend.position = "none",
          panel.spacing = unit(.75, "lines"),
          axis.text = element_text(colour = "black")) +
    labs(x = "")

  # add CIs if necessary
  if (isTRUE(ci)) {
    plot_a <- plot_a +
      geom_pointrange(alpha = 0.5,
                      size = .15)   # points are size * 4 (= .6), as before
  }

  # add ribbon if necessary
  if (isTRUE(ribbon)) {
    plot_a <- plot_a +
      geom_ribbon(aes(ymin = .data$conf.low,
                      ymax = .data$conf.high,
                      color = "lightgrey"),
                  alpha = 0.25)
  }

  }

  # Create choice panel (only if needed)
  value <- key <- NULL

  if (type %in% c("default", "choices")) {

  plot_b <- x$data %>%
    format_results(var = var, group = group, null = null, desc = desc) %>%
    tidyr::pivot_longer(all_of(choices), names_to = "key", values_to = "value") %>%
    dplyr::mutate(key = factor(key, levels = choices)) %>%
    ggplot(aes(x = .data$specifications,
               y = value,
               color = .data$color)) +
    geom_point(aes(x = .data$specifications,
                   y = value),
               shape = 124,
               size = 3.35) +
    scale_color_identity() +
    theme_minimal() +
    facet_grid(key~1, scales = "free_y", space = "free_y") +
    theme(
      axis.line = element_line("black", linewidth = .5),
      legend.position = "none",
      panel.spacing = unit(.75, "lines"),
      axis.text = element_text(colour = "black"),
      strip.text.x = element_blank()) +
    labs(x = "", y = "")

  # Names of the choices on the left, next to their options
  if (choice_labels == "left") {
    plot_b <- plot_b +
      facet_grid(key~1, scales = "free_y", space = "free_y", switch = "y") +
      theme(strip.placement = "outside",
            strip.text.y.left = element_text(angle = 90, face = "bold"))
  }

  }

  if(type == "default"){

  p <- plot_grid(plot_a,
                 plot_b,
                 labels = labels,
                 align = "v",
                 axis = "rbl",
                 rel_heights = rel_heights,
                 ncol = 1.)
   return(p)
  }

  if(type == "curve") {

    return(plot_a)

  }

  if(type == "choices") {

    return(plot_b)

  }

  if(type == "boxplot") {

    plot_c <- x$data %>%
      tidyr::pivot_longer(all_of(choices), names_to = "key", values_to = "value") %>%
      ggplot(aes(x = value,
                 y = !! var,
                 fill = key)) +
      geom_boxplot(outlier.color = "red") +
      coord_flip() +
      scale_fill_brewer(palette = "Blues") +
      facet_grid(key~1, scales = "free_y", space = "free") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.line = element_line("black", linewidth = .5),
            axis.text = element_text(colour = "black"),
            strip.text.x = element_blank()) +
      labs(x = "")

    return(plot_c)
  }

  if(type == "variance") {

   if(is.null(formula)) {

    # Only choices with more than one option in the results can be random effects
    # (counted in the results, so that filtered specifications are respected)
    n_options <- function(choice) length(unique(x$data[[choice]]))

    if(n_options("x") > 1) {
      var_x = "+ (1|x)"
    } else {
      var_x = ""
    }

    if(n_options("y") > 1) {
      var_y = "+ (1|y)"
    } else {
      var_y = ""
    }

    if(n_options("model") > 1) {
      var_model = "+ (1|model)"
    } else {
      var_model = ""
    }

    if(n_options("controls") > 1) {
      var_controls = "+ (1|controls)"
    } else {
      var_controls = ""
    }

    if(n_options("subsets") > 1) {
      var_subsets = "+ (1|subsets)"
    } else {
      var_subsets = ""
    }

    if(all(c(var_x, var_y, var_model, var_controls, var_subsets) == "")) {
      stop("The variance decomposition requires at least one analytical choice with more than one option.")
    }

    formula <- paste("estimate ~ 1", var_x, var_y, var_model, var_controls, var_subsets)

   }

    model <- lme4::lmer(formula = formula, data = x$data)

    # only variances (no covariances of random slopes in custom formulas)
    var <- lme4::VarCorr(model) %>%
      as.data.frame %>%
      filter(is.na(.data$var2)) %>%
      select("grp", "vcov")

    # sum up all variance components
    sum_var <- sum(var$vcov)

    # estimate icc
    var <- var %>%
      mutate(icc = .data$vcov/sum_var,
             percent = .data$icc*100)

    if(isTRUE(print)) {

      var %>%
        mutate_if(is.numeric, round, 2) %>%
        print

    }

    plot_d <- ggplot(var, aes(x = .data$grp,
                    y = .data$percent)) +
      geom_bar(stat = "identity", fill = "#377eb8") +
      theme_minimal() +
      theme(axis.text = element_text(colour = "black"),
            axis.line.y = element_line(colour = "black"),
            axis.line.x = element_line(colour = "black")) +
      labs(x = "", y = "proportion of variance")

    return(plot_d)

  }

  if(type == "samplesizes") {

    if (!"fit_nobs" %in% names(x$data)) {
      stop("The results contain no sample sizes (`fit_nobs`), which are extracted with `fun2` in setup().")
    }

    plot_e <- x$data %>%
      format_results(var = var, group = group, desc = desc) %>%
      ggplot(aes(x = .data$specifications,
                 y = .data$fit_nobs)) +
      geom_col(fill = "grey") +
      theme_minimal() +
      theme(
        axis.line = element_line("black", linewidth = .5),
        legend.position = "none",
        panel.spacing = unit(.75, "lines"),
        axis.text = element_text(colour = "black")) +
      labs(x = "", y = "")

    return(plot_e)

  }
}


#' Return tibble from specr.object
#' @keywords internal
#' @export
as_tibble.specr.object <- function(x,...) {
  as_tibble(x$data, ...)
}


#' Return data.frame from specr.object
#' @keywords internal
#' @export
as.data.frame.specr.object <- function(x, ...) {
  as.data.frame(x$data, ...)
}




#' Print method for S3 class "specr.infer"
#' @keywords internal
#' @export
print.specr.infer <- function(x, ...) {

  # Short technical summary
  cat("Inference on the specification curve (resampling under the null)\n")
  cat("-------------------\n")

  cat("Technical details:\n\n")
  cat("  Class:                          ", class(x), "-- version:", as.character(utils::packageVersion("specr")), "\n")
  cat("  Cores used:                     ", x$workers, "\n")
  cat("  Duration of fitting process:    ", x$time, "\n")
  cat("  Number of specifications:       ", x$n_specs, "\n")
  cat("  Number of bootstrap samples:    ", x$n_samples, "\n")
  if (x$n_failed > 0) {
    cat("  Failed refits (ignored):        ", x$n_failed, "\n")
  }

  cat("\nTest statistics (two-sided, see ?summary.specr.infer):\n\n")
  print(summary(x))

  invisible(x)
}

#' Summarizing inference on the specification curve
#'
#' @description Computes the three test statistics proposed by Simonsohn et al.
#'    (2020) for the observed specification curve and for each curve under the null
#'    (resulting from [infer_curve()]), and tests how extreme the observed statistics
#'    are compared to their distribution under the null.
#'
#' @param object A `specr.infer` object, resulting from calling [infer_curve()].
#' @param direction Direction of the alternative hypothesis. The default
#'    `"two.sided"` tests for an effect in either direction. Use `"greater"` or
#'    `"less"` only if the direction of the effect was predicted before the results
#'    were known.
#' @param alpha Significance level that is used to count significant
#'    specifications. Defaults to .05.
#' @param group Optional character vector with names of analytical choices (e.g.,
#'    `"y"` or `"subsets"`) to compute the test statistics separately within each
#'    group of specifications.
#' @param ... Further arguments passed to or from other methods (currently ignored).
#'
#' @details The three test statistics are:
#'    \describe{
#'      \item{median}{The median estimate across all specifications.}
#'      \item{share significant}{The number of specifications with a significant
#'        estimate (p < `alpha`) in the tested direction: positive for `"greater"`,
#'        negative for `"less"`, and the more frequent (dominant) direction for
#'        `"two.sided"`.}
#'      \item{aggregated z}{Stouffer's Z: each p-value is converted into a z-value
#'        with the sign of its estimate, and the sum of these z-values is divided by
#'        the square root of the number of specifications.}
#'    }
#'    The p-value of each statistic is the share of curves under the null whose
#'    statistic is at least as extreme as the observed one, computed as (1 + number
#'    of curves at least as extreme) / (1 + number of bootstrap samples), so that it
#'    is never zero. For `"two.sided"`, "at least as extreme" refers to the absolute
#'    value of the median and of Stouffer's Z, and to the number of significant
#'    specifications in the dominant direction of each curve. If the extraction
#'    function did not return p-values, only the median can be tested.
#'
#' @return A tibble with one row per test statistic (and group), including the
#'    observed `estimate`, the 2.5% and 97.5% quantiles of the statistic under the
#'    null (`null_lower` and `null_upper`), the `p.value`, and the number of
#'    specifications (`n_specs`).
#'
#' @references \itemize{
#'  \item Simonsohn, U., Simmons, J.P. & Nelson, L.D. (2020). Specification curve analysis. *Nature Human Behaviour, 4*, 1208–1214. https://doi.org/10.1038/s41562-020-0912-z
#' }
#'
#' @seealso [infer_curve()], [plot.specr.infer()]
#'
#' @export
#'
#' @examples
#' specs <- setup(data = example_data,
#'    y = "y1",
#'    x = c("x1", "x2"),
#'    model = "lm",
#'    controls = c("c1", "c2"))
#'
#' results <- specr(specs)
#'
#' set.seed(42)
#' inference <- infer_curve(results, specs, n_samples = 200) # better 1,000 or more
#'
#' # Two-sided tests (default)
#' summary(inference)
#'
#' # One-sided tests for a predicted positive effect
#' summary(inference, direction = "greater")
#'
#' # Separately for each independent variable
#' summary(inference, group = "x")
summary.specr.infer <- function(object,
                                direction = c("two.sided", "greater", "less"),
                                alpha = .05,
                                group = NULL,
                                ...) {

  direction <- match.arg(direction)
  observed <- object$observed

  if (is.null(group)) {

    groups <- list(seq_len(nrow(observed)))

  } else {

    if (!all(group %in% names(observed))) {
      stop("`group` must contain names of analytical choices, e.g. \"x\", \"y\", \"controls\", or \"subsets\".")
    }
    groups <- split(seq_len(nrow(observed)),
                    interaction(as.data.frame(observed[group]), drop = TRUE, lex.order = TRUE))
  }

  res <- lapply(groups, function(cols) {
    out <- curve_inference(observed$estimate[cols],
                           observed$p.value[cols],
                           object$estimates[, cols, drop = FALSE],
                           object$p_values[, cols, drop = FALSE],
                           direction = direction,
                           alpha = alpha)
    if (!is.null(group)) {
      out <- dplyr::bind_cols(observed[rep(cols[1], nrow(out)), group], out)
    }
    out
  })

  dplyr::bind_rows(res)
}


#' Plot specification curve against curves under the null
#'
#' @description This function plots the observed specification curve (estimates
#'   ranked from lowest to highest) on top of the median and the 2.5% and 97.5%
#'   quantiles of the ranked curves under the null (resulting from [infer_curve()]).
#'
#' @param x A `specr.infer` object, resulting from calling [infer_curve()].
#' @param ... further arguments passed to or from other methods (currently ignored).
#'
#' @return A \link[ggplot2]{ggplot} object that can be customized further.
#'
#' @seealso [infer_curve()], [summary.specr.infer()]
#'
#' @export
plot.specr.infer <- function(x, ...) {

  rank <- value <- curve <- NULL

  n_specs <- x$n_specs

  # Rank the estimates within each curve under the null
  sorted <- if (n_specs == 1) {
    x$estimates
  } else {
    t(apply(x$estimates, 1, sort, na.last = TRUE))
  }
  quantiles <- apply(sorted, 2, stats::quantile, probs = c(.025, .5, .975), na.rm = TRUE)

  curves <- c("observed", "null: median", "null: 2.5%", "null: 97.5%")

  df <- tibble::tibble(
    rank = rep(seq_len(n_specs), times = 4),
    curve = factor(rep(curves, each = n_specs), levels = curves),
    value = c(sort(x$observed$estimate), quantiles[2, ], quantiles[1, ], quantiles[3, ]))

  ggplot(df, aes(x = rank, y = value, color = curve, linetype = curve)) +
    geom_line(linewidth = .75) +
    scale_color_manual(values = c("steelblue", "grey40", "grey40", "grey40")) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotted")) +
    theme_minimal() +
    theme(
      axis.line = element_line("black", linewidth = .5),
      panel.spacing = unit(.75, "lines"),
      axis.text = element_text(colour = "black")
    ) +
    labs(x = "Specifications (ranked)", y = "estimate", color = "", linetype = "")

}


#' Return tibble from specr.infer
#'
#' Long format with one row per bootstrap sample and specification.
#' @keywords internal
#' @export
as_tibble.specr.infer <- function(x, ...) {

  keys <- setdiff(names(x$observed), c("estimate", "std.error", "p.value"))

  dplyr::bind_cols(
    tibble::tibble(sample = rep(seq_len(x$n_samples), each = x$n_specs)),
    x$observed[rep(seq_len(x$n_specs), times = x$n_samples), keys],
    tibble::tibble(estimate = as.vector(t(x$estimates)),
                   p.value = as.vector(t(x$p_values))))
}


#' Return data.frame from specr.infer
#' @keywords internal
#' @export
as.data.frame.specr.infer <- function(x, ...) {
  as.data.frame(as_tibble(x), ...)
}
