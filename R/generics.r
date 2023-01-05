#' Summarizing the Specifications Setup
#'
#' \code{summary} method for class "specr.setup". Provides a short summary of the
#'    created specifications (the "multiverse") that lists all analytic choices, prints
#'    the function used to extract the parameters from the model. Finally, if
#'    \code{print.specs = TRUE}, it also shows the head of the actual specification grid.
#'
#' @param x An object of class "specr.setup", usually, a result of a call to `setup`.
#' @param digits The number of digits to use when printing the specification table.
#' @param rows The number of rows of the specification tibble that should be printed.
#' @param print.specs Logical value; if `TRUE`, a head of the specification tibble
#'   is returned and printed.
#'
#' @return A printed summary of an object of class \code{specr.setup}.
#'
#' @export
#'
#' @examples
#' # Setup specifications
#' specs <- setup(data = example_data,                   # data
#'                x = c("x1", "x2"),                     # independent variables
#'                y = c("y1"),                           # dependent variable
#'                model = c("lm", "glm"),                # model functions
#'                controls = c("c1", "c2", "c3"))        # covariates to be added
#' summary(specs)
#' summary(specs, rows = 10)
#' @seealso The function used to create the "specr.setup" object, `setup`.
summary.specr.setup <- function(x,
                                digits = 2,
                                rows = 6,
                                print.specs = TRUE) {

  cat("Setup for the Specification Curve Analysis\n")
  cat("-------------------------------------------\n")
  cat("Class:\t\t\t\t", class(x), "-- version:", as.character(packageVersion("specr")), "\n")
  cat("Number of specifications:\t", as.numeric(x$n_specs), "\n\n")


  cat("Specifications:\n\n")
  cat("  Independent variable:\t\t", paste0(unique(x$x), collapse = ", "), "\n")
  cat("  Dependent variable:\t\t", paste0(unique(x$y), collapse = ", "), "\n")
  cat("  Models:\t\t\t", paste0(unique(x$model), collapse = ", "), "\n")
  cat("  Covariates:\t\t\t", sub("\\b1\\b", "1 (none)", paste0(unique(x$specs$controls), collapse = ", ")), "\n")

  cat("  Subsets analyses:\t\t", paste0(unique(x$specs$subsets), collapse = ", "), "\n\n")


  cat("Function used to extract parameters:\n\n")
  cat("  ")
  print(x$fun)

  if(isTRUE(print.specs)) {
  cat("\n\nHead of specifications table (first", rows, "rows):\n\n")
  x$specs %>%
    dplyr::select(-model_function) %>%
    dplyr::mutate_if(is.numeric, round, digits) %>%
    head(n = rows)
  }
}


#' Summarizing the Specification Curve Analysis
#'
#' `summary` method for class "specr". It provides a printed output including
#'   technical details (e.g., cores used, duration of the fitting process, number
#'   of specifications), a descriptive analysis of the overall specification curve,
#'   a descriptive summary of the resulting sample sizes, and a head of the results.
#'
#' @param x An object of class "specr", usually resulting of a call to `specr`.
#' @param what Different aspects can be summarized and printed. See details for alternative summaries
#' @param ... In combination with `what = "curve"`, provide one or more variables (e.g., subsets, controls,...) that denote the available analytic choices to group summary of the estimate.
#' @param stats Named vector or named list of summary functions (individually defined summary functions can included). If it is not named, placeholders (e.g., "fn1") will be used as column names.
#' @param digits The number of digits to use when printing the specification table.
#' @param rows The number of rows of the specification tibble that should be printed.
#'
#' @return A printed summary of an object of class \code{specr.object}.
#'
#' @export
#'
#' @examples
#' # Setup up specifications (returns "specr.setup" object)
#' specs <- setup(data = example_data,
#'                y = c("y1", "y2"),
#'                x = c("x1", "x2"),
#'                model = "lm",
#'                distinct(example_data, group1),
#'                controls = c("c1", "c2"))
#'
#' # Run analysis (returns "specr" object)
#' results <- specr(specs, workers = 1)
#'
#' # Summary of the "specr" object
#' summary(results)  # Default
#' summary(results, digits = 5, rows = 10)
#' summary(results, type = "curve")
#' summary(results, type = "curve", x, y)
#' summary(results, type = "curve",
#'        x, group1,
#'        stats = list(median = median,
#'                     min = min,
#'                     max = max))
#' @seealso The function used to create the "specr.setup" object: `setup`.
summary.specr.object <- function(x,
                          type = "default",
                          ...,
                          var = estimate,
                          stats = list(median = median, mad = mad, min = min, max = max,
                                       q25 = function(x) quantile(x, prob = .25),
                                       q75 = function(x) quantile(x, prob = .75)),
                          digits = 2,
                          rows = 6){

  var <- enquo(var)
  group_var <- enquos(...)

  if(type == "default") {

    # Short technical summary
    cat("Results of the specification curve analysis\n")
    cat("-------------------\n")

    cat("Technical details:\n\n")
    cat("  Class:\t\t\t", class(x), "-- version:", as.character(packageVersion("specr")), "\n")
    cat("  Cores used:\t\t\t", x$workers, "\n")
    cat("  Duration of fitting process:\t", x$time$callback_msg, "\n")
    cat("  Number of specifications:\t", as.numeric(x$n_specs), "\n\n")

    # Short descriptive analysis across all specifications
    cat("Descriptive summary of the specification curve:\n\n")

    x$data %>%
      summarize_at(vars(!! var), stats) %>%
      as.data.frame %>%
      round(digits) %>%
      print(row.names = FALSE)

    cat("\n")

    # Head of the result table
    cat("Descriptive summary of sample sizes: \n\n")
    des2 <- x$data %>%
      summarize(median = median(fit_nobs),
                min = min(fit_nobs),
                max = max(fit_nobs)) %>%
      as.data.frame %>%
      round(digits)
    print(des2, row.names = FALSE)

    cat("\n")

    # Head of the result table
    cat("Head of the specification results (first", rows, "rows): \n\n")
    x$data %>%
      dplyr::select(-model_function, -term) %>%
      dplyr::mutate_if(is.numeric, round, digits) %>%
      head(n = rows) %>%
      print
  }

  if(type == "curve") {

    if (length(group_var) == 0) {

      dplyr::bind_cols(
        x$data %>%
          summarize_at(vars(!! var), stats),
        x$data %>%
          dplyr::summarize(obs = median(.data$fit_nobs))
      )

    } else {

      dplyr::left_join(
        x$data %>%
          dplyr::group_by(!!! group_var) %>%
          summarize_at(vars(!! var), stats),
        x$data %>%
          dplyr::group_by(!!! group_var) %>%
          dplyr::summarize(obs = median(.data$fit_nobs)),
        by = names_from_dots(...)
      )
    }
  }

}

#' Plot specification curve and analytic choices
#'
#' @description This function plots visualizations of the specification curve
#'   analysis. The function requires an object of class \code{specr.object}, usually
#'   the results of calling \code{specr()} to create a standard visualization of the
#'   specification curve analysis. Several types of visualizations are possible.
#'
#' @param x A `specr.object` object, usually resulting from calling \code{specr()}.
#' @param type What type of figure should be plotted? If \code{what = "default"},
#'   the standard specification curve analysis plot (the specification curve as the
#'   upper panel and an overview of the relevant choices as the lower panel) is
#'   created. If \code{what = "curve"}, only the specification curve (upper panel
#'   of the default plot) is plotted. If \code{what = "choices"} only the choice
#'   panel (lower part of the default plot) is plotted. \code{what = "boxplot"},
#'   an alternative visualization of differences between choices is plotted. See
#'   examples for more information.
#' @param var Which parameter should be plotted in the curve? Defaults to
#'    \code{estimate}, but other parameters (e.g., p.value, fit_r.squared,...)
#'    can be plotted too.
#' @param choices A vector specifying which analytic choices should be plotted.
#'     By default, all choices (x, y, model, controls, subsets) are plotted.
#' @param labels Labels for the two parts of the plot
#' @param rel_heights vector indicating the relative heights of the plot.
#' @param desc Logical value indicating whether the curve should the arranged in
#'   a descending order. Defaults to FALSE.
#' @param ci Logical value indicating whether confidence intervals should be
#'   plotted.
#' @param ribbon Logical value indicating whether a ribbon instead should be
#'   plotted.
#' @param null Indicate what value represents the 'null' hypothesis (defaults to
#'   zero).
#' @param ... additional arguments that can be passed to \code{plot_grid()}.
#'
#' @return A \link[ggplot2]{ggplot} object that can be customized further.
#'
#' @export
#'
#' @examples
#' # Setup specifications
#' specs <- setup(data = example_data,
#'                y = c("y1", "y2"),
#'                x = c("x1", "x2"),
#'                model = "lm",
#'                distinct(example_data, group1),
#'                distinct(example_data, group2),
#'                controls = c("c1", "c2"))
#'
#' # Run analysis
#' results <- specr(specs, workers = 1)
#'
#' # Plot results in various ways
#' plot(results)
#' plot(results, choices = c("x", "y"))
#' plot(results, ci = FALSE, ribbon = TRUE)
#' plot(results, type = "curve", desc = TRUE)
#' plot(results, type = "boxplot") + scale_fill_brewer(palette = "Dark2")
#'
#' # Also other variables in the resulting data set can be plotted
#' plot(results, type = "curve", var = fit_r.squared, ci = FALSE)
#'
#' # Such a plot can also be extended (e.g., by again adding the estimates with
#' # confidence intervals)
#' plot(results, type = "curve", var = fit_r.squared) +
#'   geom_point(aes(y = estimate), shape = 5) +
#'   labs (x = "specifications", y = "r-squared | estimate")
#'
plot.specr.object <- function(x,
                              type = "default",
                              var = estimate,
                              choices = c("x", "y", "model", "controls", "subsets"),
                              labels = c("A", "B"),
                              rel_heights = c(2, 3),
                              desc = FALSE,
                              null = 0,
                              ci = TRUE,
                              ribbon = FALSE,
                              ...){


  var <- enquo(var)

  # Create specification curve plot
  plot_a <- x$data %>%
    format_results(var = var, null = null, desc = desc) %>%
    ggplot(aes(x = specifications,
               y = !! var,
               ymin = conf.low,
               ymax = conf.high,
               color = color)) +
    geom_point(aes(color = color),
               size = 1) +
    theme_minimal() +
    scale_color_identity() +
    theme(strip.text = element_blank(),
          axis.line = element_line("black", size = .5),
          legend.position = "none",
          panel.spacing = unit(.75, "lines"),
          axis.text = element_text(colour = "black")) +
    labs(x = "")

  # add legends if necessary
  if (isFALSE(legend)) {
    plot_a <- plot_a +
      theme(legend.position = "none")
  }

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
    format_results(var = var, null = null, desc = desc) %>%
    tidyr::gather(key, value, choices) %>%
    dplyr::mutate(key = factor(key, levels = choices)) %>%
    ggplot(aes(x = specifications,
               y = value,
               color = color)) +
    geom_point(aes(x = specifications,
                   y = value),
               shape = 124,
               size = 3.35) +
    scale_color_identity() +
    theme_minimal() +
    facet_grid(.data$key~1, scales = "free_y", space = "free_y") +
    theme(
      axis.line = element_line("black", size = .5),
      legend.position = "none",
      panel.spacing = unit(.75, "lines"),
      axis.text = element_text(colour = "black"),
      strip.text.x = element_blank()) +
    labs(x = "", y = "")

  if(type == "default"){

  p <- cowplot::plot_grid(plot_a,
                     plot_b,
                     labels = labels,
                     align = "v",
                     axis = "rbl",
                     rel_heights = rel_heights,
                     ncol = 1,
                     ...)
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
      ggplot(aes(x = value, y = estimate, fill = key)) +
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
}


#' Return tibble from specr.object
#' @export
as_tibble.specr.object <- function(x) {
  x$data
}


#' Return data.frame from specr.object
#' @export
as.data.frame.specr.object <- function(x) {
  as.data.frame(x$data)
}


#' Print technical details
#' @export
print.specr.object <- function(x) {

  cat("Technical details of pecification curve analysis\n")
  cat("-------------------\n")
  cat("  Class:\t\t\t", class(x), "-- version:", as.character(packageVersion("specr")), "\n")
  cat("  Cores used:\t\t\t", x$workers, "\n")
  cat("  Duration of fitting process:\t", x$time$callback_msg, "\n")
  cat("  Number of specifications:\t", as.numeric(x$n_specs), "\n\n")

}

