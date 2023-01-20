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
  cat("Number of specifications:  ", as.numeric(object$n_specs), "\n\n")


  cat("Specifications:\n\n")
  cat("  Independent variable:    ", paste0(unique(object$x), collapse = ", "), "\n")
  cat("  Dependent variable:      ", paste0(unique(object$y), collapse = ", "), "\n")
  cat("  Models:                  ", paste0(unique(object$model), collapse = ", "), "\n")
  cat("  Covariates:              ", sub("\\b1\\b", "1 (none)", paste0(unique(object$specs$controls), collapse = ", ")), "\n")
  cat("  Subsets analyses:        ", paste0(unique(object$specs$subsets), collapse = ", "), "\n\n")


  cat("Function used to extract parameters:\n\n")
  cat("  ")
  print(object$fun1)

  if(isTRUE(print.specs)) {
  cat("\n\nHead of specifications table (first", rows, "rows):\n\n")
  object$specs %>%
    select(-.data$model_function) %>%
    mutate_if(is.numeric, round, digits) %>%
    utils::head(n = rows)
  }
}

#' Print method for S3 class "specr.setup"
#' @keywords internal
#' @export
print.specr.setup <- function(x, ...) {

  cat("Number of specifications: ", as.numeric(x$n_specs), "\n\n")

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
    select(.data$model, .data$x, .data$y, .data$controls, .data$subsets) %>%
    arrange(.data$model, .data$x, .data$y, .data$controls, .data$subsets) %>%
    mutate(start = "raw data") %>%
    select(start, dplyr::everything()) %>%
    mutate(x = paste0(.data$x, " & ", .data$model),
           y = paste0(.data$y, " & ", .data$x),
           controls = paste0(.data$controls, " & ", .data$y),
           subsets = paste0(.data$subsets, " & ", .data$controls))

  # Create edges
  edges_level1 <- df %>%
    select(.data$start, .data$model) %>%
    rename(from = .data$start, to = .data$model) %>%
    unique %>%
    mutate(decisions = "model")
  edges_level2 <- df %>%
    select(.data$model, .data$x) %>%
    rename(from = .data$model, to = .data$x) %>%
    unique %>%
    mutate(decisions = "independent variable")
  edges_level3 <- df %>%
    select(.data$x, .data$y) %>%
    rename(from = .data$x, to = .data$y) %>%
    unique %>%
    mutate(decisions = "dependent variable")
  edges_level4 = df %>%
    select(.data$y, .data$controls) %>%
    rename(from = .data$y, to = .data$controls) %>%
    mutate(decisions = "control variables")
  edges_level5 <- df %>%
    select(.data$controls, .data$subsets) %>%
    rename(from = .data$controls, to = .data$subsets) %>%
    mutate(decisions = "subsets")

  # Combine edges
  edge_list <- rbind(edges_level1,
                     edges_level2,
                     edges_level3,
                     edges_level4,
                     edges_level5)
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
      summarize_at(vars(!! var), stats) %>%
      as.data.frame %>%
      round(digits) %>%
      print(row.names = FALSE)

    cat("\n")

    # Head of the result table
    cat("Descriptive summary of sample sizes: \n\n")
    des2 <- object$data %>%
      summarize(median = median(.data$fit_nobs),
                min = min(.data$fit_nobs),
                max = max(.data$fit_nobs)) %>%
      as.data.frame %>%
      round(digits)
    print(des2, row.names = FALSE)

    cat("\n")

    # Head of the result table
    cat("Head of the specification results (first", rows, "rows): \n\n")
    object$data %>%
      select(-.data$model_function, -.data$term) %>%
      mutate_if(is.numeric, round, digits) %>%
      utils::head(n = rows) %>%
      print
  }

  if(type == "curve") {

    if (length(group) == 0) {

      dplyr::bind_cols(
        object$data %>%
          summarize_at(vars(!! var), stats),
        object$data %>%
          dplyr::summarize(obs = median(.data$fit_nobs))
      )

    } else {

      dplyr::left_join(
        object$data %>%
          dplyr::group_by_at(group) %>%
          summarize_at(vars(!! var), stats),
        object$data %>%
          dplyr::group_by_at(group) %>%
          dplyr::summarize(obs = median(.data$fit_nobs)),
        by = group
      )
    }
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
    summarize_at(vars(.data$estimate),
                 list(median = median, mad = mad, min = min, max = max,
                      q25 = function(x) quantile(x, prob = .25),
                      q75 = function(x) quantile(x, prob = .75))) %>%
    as.data.frame %>%
    round(2) %>%
    print(row.names = FALSE)

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
                              ...){


  var <- enquo(var)
  group <- enquo(group)

  # Create specification curve plot
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
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          panel.spacing = unit(.75, "lines"),
          axis.text = element_text(colour = "black")) +
    labs(x = "")

  # add CIs if necessary
  if (isTRUE(ci)) {
    plot_a <- plot_a +
      geom_pointrange(alpha = 0.5,
                      size = .6,
                      fatten = 1)
  }

  # add ribbon if necessary
  if (isTRUE(ribbon)) {
    plot_a <- plot_a +
      geom_ribbon(aes(ymin = .data$conf.low,
                      ymax = .data$conf.high,
                      color = "lightgrey"),
                  alpha = 0.25)
  }


  # Create choice panel
  value <- key <- NULL

  plot_b <- x$data %>%
    format_results(var = var, group = group, null = null, desc = desc) %>%
    tidyr::gather(key, value, choices) %>%
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
      axis.line = element_line("black", size = .5),
      legend.position = "none",
      panel.spacing = unit(.75, "lines"),
      axis.text = element_text(colour = "black"),
      strip.text.x = element_blank()) +
    labs(x = "", y = "")

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
      tidyr::gather(key, value, all_of(choices)) %>%
      ggplot(aes(x = value,
                 y = !! var,
                 fill = key)) +
      geom_boxplot(outlier.color = "red") +
      coord_flip() +
      scale_fill_brewer(palette = "Blues") +
      facet_grid(key~1, scales = "free_y", space = "free") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.line = element_line("black", size = .5),
            axis.text = element_text(colour = "black"),
            strip.text.x = element_blank()) +
      labs(x = "")

    return(plot_c)
  }

  if(type == "variance") {

  grp <- NULL

   if(is.null(formula)) {

    if(length(x$x) > 1) {
      var_x = "+ (1|x)"
    } else {
      var_x = ""
    }

    if(length(x$y) > 1) {
      var_y = "+ (1|y)"
    } else {
      var_y = ""
    }

    if(length(x$model) > 1) {
      var_model = "+ (1|model)"
    } else {
      var_model = ""
    }

    if(length(x$controls) > 0) {
      var_controls = "+ (1|controls)"
    } else {
      var_controls = ""
    }

    if(x$subsets[1] == "none") {
      var_subsets = ""
    } else {
      var_subsets = "+ (1|subsets)"
    }

    formula <- paste("estimate ~ 1", var_x, var_y, var_model, var_controls, var_subsets)

   }

    model <- lme4::lmer(formula = formula, data = x$data)

    var <- lme4::VarCorr(model) %>%
      as.data.frame %>%
      select(grp, vcov)

    # sum up all variance components
    sum_var <- sum(var$vcov)

    # estimate icc
    var <- var %>%
      mutate(icc = vcov/sum_var,
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
      labs(x = "", y = "proportion of variance", fill = "analytical choices")

    return(plot_d)

  }

  if(type == "samplesizes") {

    plot_e <- x$data %>%
      format_results(var = var, group = group, desc = desc) %>%
      ggplot(aes(x = .data$specifications,
                 y = .data$fit_nobs)) +
      geom_bar(stat = "identity",
               fill = "grey",
               size = .2) +
      theme_minimal() +
      theme(
        axis.line = element_line("black", size = .5),
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
