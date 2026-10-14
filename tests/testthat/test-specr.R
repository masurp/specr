context("specr")

specs <- specr::setup(data = example_data,
      x = c("x1", "x2"),
      y = c("y1"),
      model = "lm",
      controls = c("c1"))


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




