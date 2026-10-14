# Fixtures: see helper-specs.R
specs <- specs_subsets
results <- results_subsets

# Test 1
test_that("Function plot.specr.setup creates a ggplot", {
  p <- plot(specs)
  expect_s3_class(p, "gg")
})


# Test 2
test_that("Function plot.specr.object creates a ggplot", {
  p <- plot(results)
  expect_s3_class(p, "gg")
})


# Test 3
test_that("Function plot.specr.object with `type = 'curve'` creates a ggplot", {
  p <- plot(results, "curve")
  expect_s3_class(p, "gg")
})

# Test 4
test_that("Function plot.specr.object with `type = 'choices'` creates a ggplot", {
  p <- plot(results, "choices")
  expect_s3_class(p, "gg")
})


# Test 5
test_that("Function plot.specr.object with `type = 'boxplot'` creates a ggplot", {
  p <- plot(results, "boxplot")
  expect_s3_class(p, "gg")
})


# Test 5b
test_that("Function plot.specr.object with `type = 'samplesizes'` creates a ggplot", {
  p <- plot(results, "samplesizes")
  expect_s3_class(p, "gg")
  expect_equal(sort(p$data$fit_nobs), sort(results$data$fit_nobs))
})



# Test 6
test_that("specr.object stores the control variables", {
  expect_equal(results$controls, specs$controls)
})

# Test 7
test_that("Variance decomposition only uses choices with more than one option", {
  expect_output(p <- plot(results, type = "variance"), "grp")
  expect_s3_class(p, "gg")

  # No control variables
  results_no_controls <- specr(specr::setup(data = example_data,
                                            x = c("x1", "x2"),
                                            y = c("y1", "y2"),
                                            model = "lm"))
  expect_s3_class(plot(results_no_controls, type = "variance", print = FALSE), "gg")

  # Specifications filtered down to one independent variable
  specs_filtered <- specs
  specs_filtered$specs <- dplyr::filter(specs$specs, x == "x1")
  expect_s3_class(plot(specr(specs_filtered), type = "variance", print = FALSE), "gg")

  # Nothing varies
  results_single <- specr(specr::setup(data = example_data, x = "x1", y = "y1", model = "lm"))
  expect_error(plot(results_single, type = "variance", print = FALSE), "at least one analytical choice")
})

# Regression tests (overhaul phase 1) ----

test_that("plots work if the results contain a column named df (GitHub issue #48)", {
  results_df <- results
  results_df$data$df <- 10
  expect_s3_class(plot(results_df), "gg")
  expect_s3_class(plot(results_df, type = "curve"), "gg")
})

test_that("boxplot and variance plots work without confidence intervals", {
  results_noci <- specr(specr::setup(data = example_data, x = c("x1", "x2"), y = c("y1", "y2"),
                                     model = "lm", fun1 = function(x) broom::tidy(x)))
  expect_s3_class(plot(results_noci, type = "boxplot"), "gg")
  expect_s3_class(plot(results_noci, type = "variance", print = FALSE), "gg")
})

test_that("invalid types are refused", {
  expect_error(plot(results, type = "bogus"), "should be one of")
  expect_error(summary(results, type = "bogus"), "should be one of")
})

test_that("group can be given quoted or unquoted", {
  unquoted <- plot(results, type = "curve", group = x)
  quoted <- plot(results, type = "curve", group = "x")
  expect_equal(quoted$data$x, unquoted$data$x)
  expect_equal(quoted$data$specifications, unquoted$data$specifications)
  expect_false(is.unsorted(quoted$data$x))
})

test_that("grouped curve summaries are ungrouped", {
  s <- summary(results, type = "curve", group = c("x", "subsets"))
  expect_false(dplyr::is_grouped_df(s))
  expect_equal(nrow(s), length(unique(paste(results$data$x, results$data$subsets))))
})

test_that("summaries and plots work without sample sizes", {
  results_nonobs <- specr(specr::setup(data = example_data, x = "x1", y = c("y1", "y2"), model = "lm",
                                       fun2 = function(x) tibble::tibble(aic = AIC(x))))
  expect_false("fit_nobs" %in% names(results_nonobs$data))
  expect_output(summary(results_nonobs), "Descriptive summary of the specification curve")
  expect_false("obs" %in% names(summary(results_nonobs, type = "curve")))
  expect_error(plot(results_nonobs, type = "samplesizes"), "no sample sizes")
})

test_that("print counts filtered specifications and print/summary return invisibly", {
  specs_filtered <- specs
  specs_filtered$specs <- dplyr::filter(specs$specs, controls != "c1 + c2")
  expect_output(print(specs_filtered), paste("Number of specifications: ", nrow(specs_filtered$specs)))
  expect_invisible(print(specs))
  expect_invisible(print(results))
  invisible(capture.output(expect_invisible(summary(specs))))
  invisible(capture.output(expect_invisible(summary(results))))
  expect_output(summary(specs), "Head of specifications table")
  expect_false(any(grepl("(none)", capture.output(summary(specs)), fixed = TRUE)))
})

# GitHub issues (overhaul phase 4) ----

test_that("choice labels can be shown on the left (GitHub issue #39)", {
  right <- plot(results, type = "choices")
  left <- plot(results, type = "choices", choice_labels = "left")
  expect_s3_class(left, "gg")
  expect_equal(left$facet$params$switch, "y")
  expect_equal(left$theme$strip.placement, "outside")
  expect_null(right$facet$params$switch)
  expect_s3_class(plot(results, choice_labels = "left"), "gg")
  expect_error(plot(results, choice_labels = "top"), "should be one of")
})

# Arguments of the plot and summary methods (overhaul phase 5) ----

layer_geoms <- function(p) purrr::map_chr(p$layers, function(l) class(l$geom)[1])

test_that("the curve can be sorted in descending order", {
  p <- plot(results, type = "curve", desc = TRUE)
  expect_false(is.unsorted(rev(p$data$estimate)))
  p <- plot(results, type = "curve")
  expect_false(is.unsorted(p$data$estimate))
})

test_that("the null value determines which specifications are highlighted", {
  n_blue <- function(p) sum(p$data$color == "#377eb8")
  expect_lt(n_blue(plot(results, type = "curve", null = .5)),
            n_blue(plot(results, type = "curve", null = 0)))
})

test_that("confidence intervals and ribbons can be switched", {
  expect_true("GeomPointrange" %in% layer_geoms(plot(results, type = "curve")))
  expect_false("GeomPointrange" %in% layer_geoms(plot(results, type = "curve", ci = FALSE)))
  expect_true("GeomRibbon" %in% layer_geoms(plot(results, type = "curve", ribbon = TRUE)))
})

test_that("specific choices can be plotted", {
  p <- plot(results, type = "choices", choices = c("x", "controls"))
  expect_equal(levels(p$data$key), c("x", "controls"))
  p <- plot(results, type = "boxplot", choices = c("x", "group1"))
  expect_setequal(p$data$key, c("x", "group1"))
})

test_that("other variables than the estimate can be plotted", {
  p <- plot(results, type = "curve", var = fit_r.squared, ci = FALSE)
  expect_equal(sort(ggplot2::layer_data(p, 1)$y), sort(results$data$fit_r.squared))
})

test_that("the variance decomposition accepts a custom formula", {
  p <- plot(results, type = "variance", print = FALSE,
            formula = "estimate ~ 1 + (1|x) + (1|controls)")
  expect_setequal(as.character(p$data$grp), c("x", "controls", "Residual"))
  expect_equal(sum(p$data$percent), 100)
  expect_output(plot(results, type = "variance", print = FALSE,
                     formula = "estimate ~ 1 + (1|x) + (1|controls)"), NA)
})

test_that("summary of the curve returns descriptive statistics", {
  s <- summary(results, type = "curve")
  expect_named(s, c("median", "mad", "min", "max", "q25", "q75", "obs"))
  expect_equal(s$median, median(results$data$estimate))
  expect_equal(s$obs, median(results$data$fit_nobs))

  s <- summary(results, type = "curve", stats = list(mean = mean), var = p.value)
  expect_named(s, c("mean", "obs"))
  expect_equal(s$mean, mean(results$data$p.value))

  s <- summary(results, type = "curve", group = "group1")
  expect_equal(nrow(s), length(unique(results$data$group1)))
})

test_that("summaries print the main information", {
  expect_output(summary(results), "Results of the specification curve analysis")
  expect_output(summary(results), "Descriptive summary of sample sizes")
  expect_output(print(results), paste("Models fitted based on", nrow(results$data)))
  expect_output(summary(specs, print.specs = FALSE), "Setup for the Specification Curve Analysis")
  expect_false(any(grepl("Head of specifications table", capture.output(summary(specs, print.specs = FALSE)))))
})

test_that("objects can be converted to tibbles and data frames", {
  expect_s3_class(as_tibble(specs), "tbl_df")
  expect_equal(nrow(as_tibble(specs)), nrow(specs$specs))
  expect_s3_class(as.data.frame(specs), "data.frame")
  expect_s3_class(as_tibble(results), "tbl_df")
  expect_equal(nrow(as.data.frame(results)), nrow(results$data))
  expect_s3_class(as.data.frame(inference), "data.frame")
  expect_equal(nrow(as.data.frame(inference)), inference$n_samples * inference$n_specs)
})

test_that("the decision tree can be drawn with different layouts", {
  expect_s3_class(plot(specs, circular = TRUE), "gg")
  expect_s3_class(plot(specs, layout = "tree"), "gg")
})
