# Fixtures: see helper-specs.R
specs <- specs_simple


# test 1: check that the function returns an object of class 'specr.object'
test_that("function returns an object of class 'specr.object'", {
  expect_true(class(specr(specs, workers = 1)) == "specr.object")
})

# test 2: check that the function returns an error when no data is provided
test_that("function returns an error when no data is provided", {
  expect_error(specr(as_tibble(specs), data = NULL),
               "You provided a tibble or data.frame with all the specifications. In that case, you also need to provide the data set that should be used for the analyses.")
})


# test 3: check that the function returns an error when an incorrect object is provided
test_that("function returns an error when an incorrect object is provided", {
  expect_error(specr(1))
})

# test 4: check that the function returns an object of class 'tibble' when no specr.setup is provided
test_that("function returns an object of class 'tibble' when no specr.setup is provided", {
  expect_true(inherits(specr(as_tibble(specs),
                             data = example_data,
                             workers = 1),
                       "tbl_df"))
})


# test 4
test_that("Specs and results have the same number of rows", {
  results <- specr(specs)
  expect_equal(nrow(specs), nrow(results))
})

# test 5
test_that("Results include confidence intervals", {
  results <- specr(specs)
  expect_true(any(c("conf.low", "conf.high") %in% names(results$data)))
})

# test 6
test_that("Subsets are fitted on the right rows, sequentially and in parallel", {
  specs_sub <- specr::setup(data = example_data,
                            x = "x1",
                            y = "y1",
                            model = "lm",
                            subsets = list(group2 = c("female", "male")))
  sequential <- specr(specs_sub)
  expect_equal(sequential$data$fit_nobs[sequential$data$subsets == "female"],
               sum(example_data$group2 == "female"))
  expect_equal(sequential$data$fit_nobs[sequential$data$subsets == "all"],
               nrow(example_data))

  skip_on_cran()
  skip_on_os("windows")
  skip_if_not(future::supportsMulticore())
  old_plan <- future::plan(future::multicore, workers = 2)
  on.exit(future::plan(old_plan), add = TRUE)
  parallel <- specr(specs_sub)
  expect_equal(parallel$data$estimate, sequential$data$estimate)
})






# Regression tests (overhaul phase 1) ----

test_that("sample sizes are extracted when fun2 = NULL", {
  results <- specr(specr::setup(data = example_data, x = "x1", y = "y1",
                                model = "lm", fun2 = NULL))
  expect_equal(results$data$fit_nobs, nrow(example_data))
  expect_false(any(c("fit_r.squared", "fit_AIC") %in% names(results$data)))
})

test_that("the duration is reported in seconds", {
  expect_match(specr(specs)$time, "^[0-9.]+ sec elapsed$")
})

test_that("specifications without a parameter for x are reported", {
  d <- data.frame(x = rep(c("A", "B", "C"), 10), y = rnorm(30), cov = rnorm(30))
  specs_factor <- specr::setup(data = d, x = "x", y = "y", model = "lm", controls = "cov")
  expect_warning(results <- specr(specs_factor), "2 of 2 specification\\(s\\) returned no estimate")
  expect_equal(nrow(results$data), 0)
})

test_that("data is ignored with a warning if a setup is provided", {
  expect_warning(results <- specr(specs, data = example_data[1:10, ]), "`data` is ignored")
  expect_true(all(results$data$fit_nobs == nrow(example_data)))
})

test_that("missing values in subset variables are excluded from subsets", {
  d <- example_data
  d$group2[1:20] <- NA
  results <- specr(specr::setup(data = d, x = "x1", y = "y1", model = "lm",
                                subsets = list(group2 = "female")))
  expect_equal(results$data$fit_nobs[results$data$subsets == "female"],
               sum(d$group2 == "female", na.rm = TRUE))
})

test_that("only factor columns that are variables in the data are used as subsets", {
  s <- specr::setup(data = example_data, x = "x1", y = "y1", model = "lm")
  s$specs$label <- factor("not a subset")
  results <- specr(s)
  expect_equal(results$data$fit_nobs, nrow(example_data))
})
