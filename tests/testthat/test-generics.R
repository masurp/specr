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
