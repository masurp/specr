# Test 1:

test_that("setup returns an object of class specr.setup", {
  expect_s3_class(specr::setup(data = example_data,
                  x = c("x1", "x2"),
                  y = c("y1", "y2"),
                  model = "lm",
                  controls = c("c1", "c2", "c3")), "specr.setup")
})

# Test 2:

test_that("setup requires a data set", {
  expect_error(specr::setup(x = c("x1", "x2"),
                     y = c("y1", "y2"),
                     model = "lm",
                     controls = c("c1", "c2", "c3")),
               "You must provide the data set that should be used in the analyses.")
})

# Test 3:

test_that("setup requires an x variable", {
  expect_error(specr::setup(data = example_data,
                     y = c("y1", "y2"),
                     model = "lm",
                     controls = c("c1", "c2", "c3"),
                     simplify = TRUE),
               "You must specify at least one independent variable `x`.")
})

# Test 4:

test_that("setup requires a y variable", {
  expect_error(specr::setup(data = example_data,
                     x = c("x1", "x2"),
                     model = "lm",
                     controls = c("c1", "c2", "c3"),
                     simplify = TRUE),
               "You must specify at least one dependent variable `y`.")
})

# Test 5:

test_that("setup requires a model function", {
  expect_error(specr::setup(data = example_data,
                     x = c("x1", "x2"),
                     y = c("y1", "y2"),
                     controls = c("c1", "c2", "c3")))
})

# Test 6a:

test_that("setup does not accept duplicates in x", {
  expect_error(specr::setup(data = example_data,
                     x = c("x1", "x2", "x2"),
                     y = c("y1", "y2"),
                     model = "lm",
                     controls = c("c1", "c2", "c3")),
               "Duplicate values in x, y, model, and controls are not allowed.")
})

# Test 6b:

test_that("setup does not accept duplicates in y", {
  expect_error(specr::setup(data = example_data,
                     x = c("x1", "x2"),
                     y = c("y1", "y2", "y1"),
                     model = "lm",
                     controls = c("c1", "c2", "c3")),
               "Duplicate values in x, y, model, and controls are not allowed.")
})

# Test6c:

test_that("setup does not accept duplicates in models", {
  expect_error(specr::setup(data = example_data,
                     x = c("x1", "x2"),
                     y = c("y1", "y2"),
                     model = c("lm", "lm", "glm"),
                     controls = c("c1", "c2", "c3")),
               "Duplicate values in x, y, model, and controls are not allowed.")
})

# Test6c:

test_that("setup does not accept duplicates in controls", {
  expect_error(specr::setup(data = example_data,
                     x = c("x1", "x2"),
                     y = c("y1", "y2"),
                     model = c("lm", "lm", "glm"),
                     controls = c("c1", "c2", "c3", "c2", "c3")),
               "Duplicate values in x, y, model, and controls are not allowed.")
})

# Test 7:

test_that("setup creates all combinations", {
  specs <- specr::setup(data = example_data,
                 x = c("x1", "x2"),
                 y = c("y1", "y2"),
                 model = "lm",
                 subsets = list(group1 = unique(example_data$group1),
                                group2 = unique(example_data$group2)))
  expect_equal(nrow(specs$specs),
               (nrow(dplyr::distinct(example_data, group1)) + 1) *
               (nrow(dplyr::distinct(example_data, group2)) + 1) *
               length(c("x1", "x2")) *
               length(c("y1", "y2")))
})

# Test 8:

test_that("setup creates a formula for each specification", {
  specs <- specr::setup(data = example_data,
                 x = c("x1", "x2"),
                 y = c("y1", "y2"),
                 model = "lm",
                 controls = c("c1", "c2", "c3"))
  expect_true(all(!is.na(specs$specs$formula)))
})

# Test 9:

test_that("setup creates a model function for each specification", {
  specs <- specr::setup(data = example_data,
                 x = c("x1", "x2"),
                 y = c("y1", "y2"),
                 model = "lm",
                 controls = c("c1", "c2", "c3"),
                 simplify = TRUE)
  expect_true(all(!is.na(specs$specs$model_function)))
})

# Test 10:

test_that("setup creates a subsets variable", {
  specs <- specr::setup(data = example_data,
                 x = c("x1", "x2"),
                 y = c("y1", "y2"),
                 model = "lm",
                 subsets = list(group1 = unique(example_data$group1)),
                 controls = c("c1", "c2", "c3"),
                 simplify = TRUE)
  expect_true(all(!is.na(specs$specs$subsets)))
})

# Test 11:

test_that("setup creates object of class `specr.setup`", {
  specs <- specr::setup(data = example_data,
                 x = c("x1", "x2"),
                 y = c("y1", "y2"),
                 model = "lm",
                 subsets = list(group1 = unique(example_data$group1)),
                 controls = c("c1", "c2", "c3"),
                 simplify = TRUE)
  expect_true(inherits(specs, "specr.setup"))
})


# Regression tests (overhaul phase 1) ----

test_that("controls are matched exactly when excluding x and y", {
  d <- example_data
  d$x10 <- d$x1
  s <- specr::setup(data = d, x = "x1", y = "y1", model = "lm",
                    controls = c("x10", "c1"))
  expect_true("x10" %in% s$specs$controls)
  expect_equal(nrow(s$specs), 4)

  # controls that are x or y are still excluded
  s2 <- specr::setup(data = example_data, x = c("x1", "x2"), y = "y1", model = "lm",
                     controls = c("x1", "c1"))
  expect_false(any(s2$specs$x == "x1" & grepl("\\bx1\\b", s2$specs$controls)))
})

test_that("specifications where x and y are the same are excluded", {
  s <- specr::setup(data = example_data, x = c("x1", "y1"), y = "y1", model = "lm")
  expect_equal(s$specs$x, "x1")
})

test_that("subset labels are built from the values only", {
  d <- example_data
  d$grp <- rep(c("DNA", "NA", "RNA"), length.out = nrow(d))
  s <- specr::setup(data = d, x = "x1", y = "y1", model = "lm",
                    subsets = list(grp = c("DNA", "NA"), group2 = "female"))
  expect_setequal(s$specs$subsets,
                  c("DNA & female", "NA & female", "female", "DNA", "NA", "all"))
  expect_equal(sum(s$specs$subsets == "all"), 1)
})

test_that("subsets are validated", {
  expect_error(specr::setup(data = example_data, x = "x1", y = "y1", model = "lm",
                            subsets = list(c("female"))),
               "must be named")
  expect_error(specr::setup(data = example_data, x = "x1", y = "y1", model = "lm",
                            subsets = list(gruop2 = "female")),
               "not in the data: gruop2")
  expect_error(specr::setup(data = example_data, x = "x1", y = "y1", model = "lm",
                            subsets = list(group2 = "female", group2 = "male")),
               "must be named")
  expect_error(specr::setup(data = example_data, x = "x1", y = "y1", model = "lm",
                            subsets = list(model = "lm")),
               "cannot be named")
  expect_error(specr::setup(data = example_data, x = "x1", y = "y1", model = "lm",
                            subsets = list(group2 = c("female", "female"))),
               "Duplicate values within `subsets`")
})

test_that("missing values among the subset values are ignored", {
  s <- specr::setup(data = example_data, x = "x1", y = "y1", model = "lm",
                    subsets = list(group2 = c("female", NA)))
  expect_equal(sort(s$specs$subsets), c("all", "female"))
})

test_that("model must be given as character", {
  expect_error(specr::setup(data = example_data, x = "x1", y = "y1", model = lm),
               "must be a character vector")
})

test_that("specifications do not keep a copy of the data", {
  d <- example_data[rep(seq_len(nrow(example_data)), 5), ]
  s <- specr::setup(data = d, x = c("x1", "x2"), y = "y1", model = "lm", controls = "c1")
  size_data <- length(serialize(d, NULL))
  # the setup contains the data once (not once more per default extraction function)
  expect_lt(length(serialize(s, NULL)), 1.5 * size_data)
  # the results contain no copy of the data
  expect_lt(length(serialize(specr(s), NULL)), size_data / 2)
})


# GitHub issues (overhaul phase 4) ----

test_that("models can be given as a named list of functions (GitHub issue #29)", {
  weighted_lm <- function(formula, data) {
    lm(formula, data = data, weights = rep(1:2, length.out = nrow(data)))
  }
  s <- specr::setup(data = example_data, x = c("x1", "x2"), y = "y1",
                    model = list(ols = lm, weighted = weighted_lm), controls = "c1")
  expect_equal(s$model, c("ols", "weighted"))
  expect_setequal(s$specs$model, c("ols", "weighted"))

  # the functions are used (no lookup by name), and lm via list equals "lm"
  results <- specr(s)
  results_string <- specr(specr::setup(data = example_data, x = c("x1", "x2"), y = "y1",
                                       model = "lm", controls = "c1"))
  expect_equal(results$data$estimate[results$data$model == "ols"], results_string$data$estimate)
  expect_false(isTRUE(all.equal(results$data$estimate[results$data$model == "weighted"],
                                results_string$data$estimate)))
})

test_that("invalid model lists are refused", {
  expect_error(specr::setup(data = example_data, x = "x1", y = "y1", model = list(lm)),
               "named list of functions")
  expect_error(specr::setup(data = example_data, x = "x1", y = "y1", model = list(ols = "lm")),
               "named list of functions")
})

test_that("non-numeric independent variables give a warning (GitHub issue #47)", {
  expect_warning(specr::setup(data = example_data, x = c("x1", "group1"), y = "y1", model = "lm"),
                 "group1 are not numeric")
  expect_silent(specr::setup(data = example_data, x = c("x1", "x2"), y = "y1", model = "lm"))
})
