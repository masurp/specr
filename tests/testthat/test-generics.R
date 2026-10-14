context("generics")

specs <- specr::setup(data = example_data,
               x = c("x1", "x2"),
               y = "y1",
               model = "lm",
               subsets = list(group1 = unique(example_data$group1)),
               controls = c("c1", "c2"))
results <- specr(specs)

# Test 1
test_that("Function plot.specr.setup creates a ggplot", {
  p <- plot(specs)
  expect_is(p, "gg")
})


# Test 2
test_that("Function plot.specr.object creates a ggplot", {
  p <- plot(results)
  expect_is(p, "gg")
})


# Test 3
test_that("Function plot.specr.object with `type = 'curve'` creates a ggplot", {
  p <- plot(results, "curve")
  expect_is(p, "gg")
})

# Test 4
test_that("Function plot.specr.object with `type = 'choices'` creates a ggplot", {
  p <- plot(results, "choices")
  expect_is(p, "gg")
})


# Test 5
test_that("Function plot.specr.object with `type = 'choices'` creates a ggplot", {
  p <- plot(results, "boxplot")
  expect_is(p, "gg")
})


# Test 4
test_that("Function plot.specr.object with `type = 'choices'` creates a ggplot", {
  p <- plot(results, "choices")
  expect_is(p, "gg")
})



# Test 6
test_that("specr.object stores the control variables", {
  expect_equal(results$controls, specs$controls)
})

# Test 7
test_that("Variance decomposition only uses choices with more than one option", {
  expect_is(plot(results, type = "variance"), "gg")

  # No control variables
  results_no_controls <- specr(specr::setup(data = example_data,
                                            x = c("x1", "x2"),
                                            y = c("y1", "y2"),
                                            model = "lm"))
  expect_is(plot(results_no_controls, type = "variance"), "gg")

  # Specifications filtered down to one independent variable
  specs_filtered <- specs
  specs_filtered$specs <- dplyr::filter(specs$specs, x == "x1")
  expect_is(plot(specr(specs_filtered), type = "variance"), "gg")

  # Nothing varies
  results_single <- specr(specr::setup(data = example_data, x = "x1", y = "y1", model = "lm"))
  expect_error(plot(results_single, type = "variance"), "at least one analytical choice")
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
