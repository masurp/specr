# Functions of the old framework (deprecated since version 1.0.0). Fixtures
# (`results_old`): see helper-specs.R

# Collect the messages of all deprecation warnings that `expr` emits
deprecations <- function(expr) {
  withr::local_options(lifecycle_verbosity = "warning")
  msgs <- character()
  withCallingHandlers(expr, lifecycle_warning_deprecated = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  msgs
}

model_old <- lme4::lmer(estimate ~ 1 + (1|x) + (1|y), data = results_old)


# Each function still works and emits exactly one deprecation warning ----

test_that("setup_specs() and run_specs() still work", {
  expect_length(deprecations(s <- setup_specs(x = c("x1", "x2"), y = "y1", model = "lm",
                                              controls = c("c1", "c2"))), 1)
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 8)

  # no second warning from setup_specs() inside run_specs()
  msgs <- deprecations(r <- run_specs(df = example_data, y = "y1", x = "x1", model = "lm"))
  expect_length(msgs, 1)
  expect_match(msgs, "setup\\(\\)")
  expect_true(all(c("estimate", "conf.low", "subsets") %in% names(r)))
})

test_that("the plot functions still work", {
  expect_length(deprecations(p <- plot_specs(results_old)), 1)
  expect_s3_class(p, "gg")
  expect_length(deprecations(p <- plot_curve(results_old)), 1)
  expect_s3_class(p, "gg")
  expect_length(deprecations(p <- plot_choices(results_old)), 1)
  expect_s3_class(p, "gg")
  expect_length(deprecations(p <- plot_samplesizes(results_old)), 1)
  expect_s3_class(p, "gg")
  expect_length(deprecations(p <- plot_summary(results_old)), 1)
  expect_s3_class(p, "gg")
  expect_length(deprecations(p <- plot_decisiontree(results_old)), 1)
  expect_s3_class(p, "gg")
  expect_length(deprecations(p <- plot_variance(model_old)), 1)
  expect_s3_class(p, "gg")
})

test_that("deprecation messages point to the replacements", {
  expect_match(deprecations(plot_curve(results_old)), 'type = "curve"', fixed = TRUE)
  expect_match(deprecations(plot_summary(results_old)), 'type = "boxplot"', fixed = TRUE)
  expect_match(deprecations(summarise_specs(results_old)), 'summary(<specr.object>, type = "curve")', fixed = TRUE)
})

test_that("summarise_specs() and icc_specs() still work", {
  expect_length(deprecations(s <- summarise_specs(results_old)), 1)
  expect_equal(names(s), c("median", "mad", "min", "max", "q25", "q75", "obs"))

  expect_length(deprecations(s <- summarise_specs(results_old, x, y)), 1)
  expect_false(dplyr::is_grouped_df(s))
  expect_equal(nrow(s), 4)

  expect_length(deprecations(icc <- icc_specs(model_old)), 1)
  expect_equal(sum(icc$icc), 1)
})


# Bug fixes ----

test_that("run_specs() validates the confidence level", {
  for (level in c(0, 1, 1.5, -1)) {
    expect_error(suppressWarnings(run_specs(df = example_data, y = "y1", x = "x1", conf.level = level)),
                 "strictly greater than 0 and less than 1")
  }
})

test_that("run_specs() fits combined subsets on the right rows", {
  d <- rbind(example_data, example_data[1:5, ])   # duplicate rows
  r <- suppressWarnings(
    run_specs(df = d, y = "y1", x = "x1", model = "lm",
              subsets = list(group1 = c("young", "old"), group2 = c("female", "male")))
  )
  combined <- r$fit_nobs[r$subsets == "group1 = young & group2 = female"]
  expect_equal(combined, sum(d$group1 == "young" & d$group2 == "female"))
  expect_setequal(r$subsets,
                  c("group1 = young", "group1 = old", "group2 = female", "group2 = male", "all",
                    "group1 = young & group2 = female", "group1 = old & group2 = female",
                    "group1 = young & group2 = male", "group1 = old & group2 = male"))
})

test_that("setup_specs() accepts an empty vector of controls", {
  s <- suppressWarnings(setup_specs(x = "x1", y = "y1", model = "lm", controls = character(0)))
  expect_equal(s$controls, "no covariates")
})

test_that("plot_summary() only relabels an unambiguous set of all covariates", {
  p <- suppressWarnings(plot_summary(results_old, choices = "controls"))
  expect_true("all covariates" %in% p$data$value)

  r <- suppressWarnings(run_specs(df = example_data, y = "y1", x = "x1", model = "lm",
                                  controls = c("c1", "c2", "c3"), all.comb = TRUE))
  p <- suppressWarnings(plot_summary(r, choices = "controls"))
  expect_false("all covariates" %in% p$data$value)
  expect_true(all(c("c1 + c2", "c1 + c2 + c3") %in% p$data$value))
})

test_that("icc_specs() ignores covariances of random slopes", {
  m <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  icc <- suppressWarnings(icc_specs(m))
  expect_equal(nrow(icc), 3)   # intercept, slope, residual (no covariance)
  expect_equal(sum(icc$icc), 1)
})
