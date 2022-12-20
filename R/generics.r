#' Summarizing the Specifications Setup
#'
#' `summary` method for class "specr.setup".
#'
#' @param x an object of class "specr.setup", usually, a result of a call to `setup`.
#' @param digits the number of digits to use when printing the specification table.
#' @param rows the number of rows of the specification tibble that should be printed.
#' @param print.specs logical; if `TRUE`, a head of the specification tibble is returned and printed.
#'
#' @export
#'
#' @examples
#' # Setup specifications
#' specs <- setup(data = example_data,    # data
#'                x = c("x1", "x2"),      # independent variables
#'                y = c("y1"),            # dependent variable
#'                model = c("lm", "glm"), # model functions
#'                controls = "c1")        # covariates to be added
#' summary(specs)
#' summary(specs, print.specs = TRUE, rows = 10)
#' @seealso The function used to create the "specr.setup" object, `setup`.
summary.specr.setup <- function(x,
                                digits = 2,
                                rows = 6,
                                print.specs = FALSE) {

  cat("Setup for the Specification Curve Analysis\n")
  cat("-------------------------------------------\n")
  cat("Class:\t\t\t\t", class(x), "-- version:", as.character(packageVersion("specr")), "\n")
  cat("Number of specifications:\t", as.numeric(x$n_specs), "\n\n")


  cat("Specifications:\n\n")
  cat("  Independent variable:\t\t", x$x, "\n")
  cat("  Dependent variable:\t\t", x$y, "\n")
  cat("  Models:\t\t\t", x$model, "\n")
  cat("  Covariates:\t\t\t", paste0(unique(x$specs$controls), sep = ", "), "\n")
  cat("  Subsets:\t\t\t", x$subsets, "(and combinations thereof)\n\n")

  cat("Function used to extract parameters:\n\n")
  cat("  ")
  print(x$tidy_f)

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
#' `summary` method for class "specr".
#'
#' @param x an object of class "specr", usually resulting of a call to `specr`.
#' @param what different aspects can be summarized and printed. See details for alternative summaries
#' @param ... In combination with `what = "curve"`, provide one or more variables (e.g., subsets, controls,...) that denote the available analytic choices to group summary of the estimate.
#' @param stats named vector or named list of summary functions (individually defined summary functions can included). If it is not named, placeholders (e.g., "fn1") will be used as column names.
#' @param digits the number of digits to use when printing the specification table.
#' @param rows the number of rows of the specification tibble that should be printed.
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
#' summary(results, what = "curve")
#' summary(results, what = "curve", x, y)
#' summary(results, what = "curve",
#'        x, group1,
#'        stats = list(median = median,
#'                     min = min,
#'                     max = max))
#' @seealso The function used to create the "specr.setup" object: `setup`.
summary.specr.object <- function(x,
                          what = "default",
                          ...,
                          stats = list(median = median, mad = mad, min = min, max = max,
                                       q25 = function(x) quantile(x, prob = .25),
                                       q75 = function(x) quantile(x, prob = .75)),
                          digits = 2,
                          rows = 6){

  if(what == "default") {

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
    des1 <- x$data %>%
      dplyr::summarize(median = median(estimate),
                mad = mad(estimate),
                min = min(estimate),
                max = max(estimate),
                q25 = quantile(estimate, prob = .25),
                q75 = quantile(estimate, prob = .75)) %>%
      as.data.frame %>%
      round(digits)
    print(des1, row.names = FALSE)

    cat("\n")

    # Head of the result table
    cat("Descriptive summary of sample sizes: \n\n")
    des2 <- x$data %>%
      dplyr::summarize(median = median(fit_nobs),
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

  if(what == "curve") {
    x$data %>%
      summarise_specs(..., var = estimate, stats = stats) %>%
      dplyr::ungroup %>%
      dplyr::mutate_if(is.numeric, round, digits) %>%
      print
  }

}

#' Plot specification curve and analytic choices
#'
#' This function plots an entire visualization of the specification curve
#' analysis. The function uses the "specr" object that is produced by \code{specr()} to creates a standard visualization of the specification curve analysis.
#'
#' @param x a "specr.object" object, usually resulting from calling \code{specr()}.
#' @param choices a vector specifying which analytical choices should be plotted. By default, all choices are plotted.
#' @param labels labels for the two parts of the plot
#' @param rel_heights vector indicating the relative heights of the plot.
#' @param desc logical value indicating whether the curve should the arranged in
#'   a descending order. Defaults to FALSE.
#' @param ci logical value indicating whether confidence intervals should be
#'   plotted.
#' @param ribbon logical value indicating whether a ribbon instead should be
#'   plotted.
#' @param null Indicate what value represents the 'null' hypothesis (defaults to
#'   zero).
#' @param ... additional arguments that can be passed to \code{plot_grid()}.
#'
#' @return a \link[ggplot2]{ggplot} object.
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
#' plot(results, ci = F, ribbon = T)
#' plot(results, what = "curve", desc = T)
#' plot(results, what = "boxplot")
plot.specr.object <- function(x,
                              what = "default",
                              choices = c("x", "y", "model", "controls", "subsets"),
                              labels = c("A", "B"),
                              rel_heights = c(2, 3),
                              desc = FALSE,
                              null = 0,
                              ci = TRUE,
                              ribbon = FALSE,
                              ...){

  # Create both plots
  plot_a <- plot_curve(x$data, ci = ci, ribbon = ribbon, desc = desc, null = null)
  plot_b <- plot_choices(x$data, choices = choices, desc = desc, null = null)

  if(what == "default"){

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

  if(what == "curve") {
    return(plot_a)
  }

  if(what == "choices") {
    return(plot_b)
  }

  if(what == "boxplot") {

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

as_tibble.specr.object <- function(x) {
  x$data
}



as.data.frame.specr.object <- function(x) {
  as.data.frame(x$data)
}


