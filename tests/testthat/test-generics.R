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

test_that("control combinations are expanded in the strike plot", {
  p <- plot(results, type = "choices", choices = "controls")

  expect_false("c1 + c2" %in% p$data$value)

  combined_specs <- unique(p$data$specifications[grepl("c1 + c2", p$data$formula, fixed = TRUE)])
  has_individual_strikes <- vapply(combined_specs, function(specification) {
    all(c("c1", "c2") %in% p$data$value[p$data$specifications == specification])
  }, logical(1))

  expect_true(all(has_individual_strikes))
})

test_that("all covariates are expanded in the strike plot when controls can be inferred", {
  specs_simple <- specr::setup(data = example_data,
                               x = c("x1", "x2"),
                               y = "y1",
                               model = "lm",
                               subsets = list(group1 = unique(example_data$group1)),
                               controls = c("c1", "c2"),
                               simplify = TRUE)
  results_simple <- specr(specs_simple)
  p <- plot(results_simple, type = "choices", choices = "controls")

  expect_false("all covariates" %in% p$data$value)

  combined_specs <- unique(p$data$specifications[grepl("c1 + c2", p$data$formula, fixed = TRUE)])
  has_individual_strikes <- vapply(combined_specs, function(specification) {
    all(c("c1", "c2") %in% p$data$value[p$data$specifications == specification])
  }, logical(1))

  expect_true(all(has_individual_strikes))
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


