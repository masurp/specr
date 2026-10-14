# Fixtures: see helper-specs.R
specs <- specs_simple


# test 1: check that the function returns an object of class 'specr.object'
test_that("function returns an object of class 'specr.object'", {
  expect_s3_class(specr(specs), "specr.object")
})

# test 2: check that the function returns an error when no data is provided
test_that("function returns an error when no data is provided", {
  expect_error(specr(as_tibble(specs), data = NULL),
               "You provided a tibble or data.frame with all the specifications. In that case, you also need to provide the data set that should be used for the analyses.")
})


# test 3: check that the function returns an error when an incorrect object is provided
test_that("function returns an error when an incorrect object is provided", {
  expect_error(specr(1), "class 'specr.setup'")
})

# test 4: check that the function returns an object of class 'tibble' when no specr.setup is provided
test_that("function returns an object of class 'tibble' when no specr.setup is provided", {
  results_tibble <- specr(as_tibble(specs), data = example_data)
  expect_s3_class(results_tibble, "tbl_df")
  expect_equal(results_tibble$estimate, specr(specs)$data$estimate)

  # also as data frame
  expect_equal(specr(as.data.frame(specs), data = example_data)$estimate,
               results_tibble$estimate)
})


# test 5
test_that("Specs and results have the same number of rows", {
  results <- specr(specs)
  expect_equal(nrow(results$data), nrow(specs$specs))
  expect_equal(results$n_specs, nrow(specs$specs))
})

# test 6
test_that("Results include confidence intervals", {
  results <- specr(specs)
  expect_true(all(c("conf.low", "conf.high") %in% names(results$data)))
})

# test 7
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
  expect_warning(specs_factor <- specr::setup(data = d, x = "x", y = "y", model = "lm", controls = "cov"),
                 "x are not numeric")
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


# Models and options (overhaul phase 5) ----

test_that("the results object contains the analytical choices", {
  results <- specr(specs)
  expect_equal(results$x, specs$x)
  expect_equal(results$y, specs$y)
  expect_equal(results$model, specs$model)
  expect_equal(results$subsets, specs$subsets)
  expect_equal(results$workers, 1)
})

test_that("glm gives the same estimates as lm", {
  results_glm <- specr(specr::setup(data = example_data, x = c("x1", "x2"), y = "y1",
                                    model = "glm", controls = "c1"))
  expect_equal(results_glm$data$estimate, specr(specs)$data$estimate)
  expect_true("fit_AIC" %in% names(results_glm$data))
})

test_that("custom model functions can be given by name", {
  assign("lm_custom", function(formula, data) lm(formula, data = data), envir = globalenv())
  withr::defer(rm("lm_custom", envir = globalenv()))
  results_custom <- specr(specr::setup(data = example_data, x = c("x1", "x2"), y = "y1",
                                       model = "lm_custom", controls = "c1"))
  expect_equal(results_custom$data$estimate, specr(specs)$data$estimate)
})

test_that("other confidence levels can be extracted with a custom fun1", {
  results_99 <- specr(specr::setup(data = example_data, x = c("x1", "x2"), y = "y1",
                                   model = "lm", controls = "c1",
                                   fun1 = function(x) broom::tidy(x, conf.int = TRUE, conf.level = .99)))
  results_95 <- specr(specs)
  expect_true(all(results_99$data$conf.high - results_99$data$conf.low >
                    results_95$data$conf.high - results_95$data$conf.low))
})

test_that("multilevel models can be fitted", {
  skip_if_not_installed("broom.mixed")
  results_lmer <- specr(specr::setup(data = example_data, x = c("x1", "x2"), y = "y1",
                                     model = "lmer", controls = "c1",
                                     add_to_formula = "(1|group1)",
                                     fun1 = function(x) broom.mixed::tidy(x, conf.int = TRUE),
                                     fun2 = NULL))
  expect_equal(nrow(results_lmer$data), 4)
  expect_true(all(results_lmer$data$term == results_lmer$data$x))
})

test_that("structural equation models keep the regression of y on x", {
  skip_if_not_installed("lavaan")
  sem_model <- function(formula, data) lavaan::sem(formula, data = data)
  results_sem <- specr(specr::setup(data = example_data, x = c("x1", "x2"), y = "y1",
                                    model = list(sem = sem_model), controls = "c1"))
  expect_equal(nrow(results_sem$data), 4)
  expect_true(all(results_sem$data$term == paste(results_sem$data$y, "~", results_sem$data$x)))
  expect_equal(results_sem$data$estimate, specr(specs)$data$estimate, tolerance = 1e-6)
})

test_that("specifications can be fitted in separate R sessions", {
  skip_on_cran()
  old_plan <- future::plan(future::multisession, workers = 2)
  on.exit(future::plan(old_plan), add = TRUE)
  results_parallel <- specr(specs)
  expect_equal(results_parallel$workers, 2)
  expect_equal(results_parallel$data$estimate, specr(specs)$data$estimate)
})
