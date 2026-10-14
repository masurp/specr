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
                     controls = c("c1", "c2", "c3")),
               "at least one model function")
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

# Test 6d:

test_that("setup does not accept duplicates in controls", {
  expect_error(specr::setup(data = example_data,
                     x = c("x1", "x2"),
                     y = c("y1", "y2"),
                     model = "lm",
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
  expect_false(any(is.na(specs$specs$formula)))
  expect_true(all(c("y1 ~ x1 + 1", "y2 ~ x2 + c1 + c2 + c3") %in% specs$specs$formula))
})

# Test 9:

test_that("setup creates a model function for each specification", {
  specs <- specr::setup(data = example_data,
                 x = c("x1", "x2"),
                 y = c("y1", "y2"),
                 model = "lm",
                 controls = c("c1", "c2", "c3"),
                 simplify = TRUE)
  expect_true(all(purrr::map_lgl(specs$specs$model_function, is.function)))
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
  expect_setequal(specs$specs$subsets, c(unique(example_data$group1), "all"))
  expect_s3_class(specs$specs$group1, "factor")
})

# Test 11:

test_that("setup creates a specr.setup object with all elements", {
  specs <- specr::setup(data = example_data,
                 x = c("x1", "x2"),
                 y = c("y1", "y2"),
                 model = "lm",
                 subsets = list(group1 = unique(example_data$group1)),
                 controls = c("c1", "c2", "c3"),
                 simplify = TRUE)
  expect_s3_class(specs, "specr.setup")
  expect_named(specs, c("specs", "n_specs", "x", "y", "model", "controls", "subsets",
                        "fun1", "fun2", "add_to_formula", "data"))
  expect_equal(specs$n_specs, nrow(specs$specs))
  expect_identical(specs$data, example_data)
  expect_equal(specs$subsets, "group1")
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


# Arguments of setup() (overhaul phase 5) ----

test_that("controls are expanded to all combinations or simplified", {
  all_combinations <- specr::setup(data = example_data, x = "x1", y = "y1", model = "lm",
                                   controls = c("c1", "c2", "c3"))
  expect_setequal(all_combinations$specs$controls,
                  c("no covariates", "c1", "c2", "c3", "c1 + c2", "c1 + c3", "c2 + c3", "c1 + c2 + c3"))

  simplified <- specr::setup(data = example_data, x = "x1", y = "y1", model = "lm",
                             controls = c("c1", "c2", "c3"), simplify = TRUE)
  expect_setequal(simplified$specs$controls,
                  c("no covariates", "c1", "c2", "c3", "all covariates"))
  expect_true("y1 ~ x1 + c1 + c2 + c3" %in% simplified$specs$formula)

  single <- specr::setup(data = example_data, x = "x1", y = "y1", model = "lm",
                         controls = "c1", simplify = TRUE)
  expect_setequal(single$specs$controls, c("no covariates", "c1"))
})

test_that("sets of controls are kept together", {
  s <- specr::setup(data = example_data, x = "x1", y = "y1", model = "lm",
                    controls = c("c1 + c2", "c3 + c4"))
  expect_setequal(s$specs$controls, c("no covariates", "c1 + c2", "c3 + c4", "c1 + c2 + c3 + c4"))
  expect_true("y1 ~ x1 + c1 + c2" %in% s$specs$formula)
})

test_that("specifications without controls have an intercept-only control part", {
  s <- specr::setup(data = example_data, x = "x1", y = "y1", model = "lm")
  expect_equal(as.character(s$specs$formula), "y1 ~ x1 + 1")
  expect_equal(s$specs$controls, "no covariates")
})

test_that("add_to_formula is added to all formulas, but not to the controls", {
  s <- specr::setup(data = example_data, x = c("x1", "x2"), y = "y1", model = "lm",
                    controls = c("c1", "c2"), add_to_formula = "c3")
  expect_true(all(endsWith(as.character(s$specs$formula), "+ c3")))
  expect_false(any(grepl("c3", s$specs$controls)))
  expect_equal(s$add_to_formula, "c3")
})

test_that("several subset variables are combined", {
  s <- specr::setup(data = example_data, x = "x1", y = "y1", model = "lm",
                    subsets = list(group1 = c("young", "old"),
                                   group2 = c("female", "male")))
  expect_setequal(s$specs$subsets,
                  c("young & female", "old & female", "young & male", "old & male",
                    "young", "old", "female", "male", "all"))
  expect_s3_class(s$specs$group1, "factor")
  expect_s3_class(s$specs$group2, "factor")
  expect_equal(s$subsets, c("group1", "group2"))
})

test_that("custom extraction functions are stored and used", {
  f1 <- function(x) broom::tidy(x)
  s <- specr::setup(data = example_data, x = "x1", y = "y1", model = "lm",
                    fun1 = f1, fun2 = NULL)
  expect_identical(s$fun1, f1)
  expect_null(s$fun2)
  out <- s$specs$model_function[[1]](formula = s$specs$formula[[1]], data = example_data)
  expect_false("conf.low" %in% names(out))
  expect_equal(grep("^fit_", names(out), value = TRUE), "fit_nobs")

  # by default, fit indices are added with the prefix fit_
  s2 <- specr::setup(data = example_data, x = "x1", y = "y1", model = "lm")
  out2 <- s2$specs$model_function[[1]](formula = s2$specs$formula[[1]], data = example_data)
  expect_true(all(c("conf.low", "fit_r.squared", "fit_nobs") %in% names(out2)))
})

test_that("several models are crossed with all other choices", {
  one <- specr::setup(data = example_data, x = c("x1", "x2"), y = "y1", model = "lm",
                      controls = "c1")
  two <- specr::setup(data = example_data, x = c("x1", "x2"), y = "y1", model = c("lm", "glm"),
                      controls = "c1")
  expect_equal(nrow(two$specs), 2 * nrow(one$specs))
})
