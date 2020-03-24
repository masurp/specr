context("run_specs: Main function")

test_that("run_specs returns tibble", {

  # Complete example
  result1 <-run_specs(df = example_data,
                      y = c("y1", "y2"),
                      x = c("x1", "x2"),
                      model = c("lm"),
                      controls = c("c1", "c2"),
                      subsets = list(group1 = unique(example_data$group1),
                                     group2 = unique(example_data$group2)))

  # Without subsets
  result2 <-run_specs(df = example_data,
                      y = c("y1", "y2"),
                      x = c("x1", "x2"),
                      model = "glm",
                      controls = c("c1", "c2"))

  # Alternative data set
  result3 <-run_specs(df = mtcars,
                      y = c("mpg"),
                      x = c("disp"),
                      model = "lm",
                      controls = c("carb", "cyl"),
                      subsets = list(gear = unique(mtcars$gear)))


  expect_true(tibble::is_tibble(result1))
  expect_true(tibble::is_tibble(result2))
  expect_true(tibble::is_tibble(result3))
})


test_that("run_specs returns stop info", {

  expect_error(run_specs(df = example_data),
               "You must specify at least one independent variable `x`.")

  expect_error(run_specs(df = example_data,
                      y = c("y1", "y2")),
               "You must specify at least one independent variable `x`.")

  expect_error(run_specs(df = example_data,
                         x = "x1"),
               "You must specify at least one dependent variable `y`.")

  expect_error(run_specs(df = example_data,
                         x = "x1",
                         y = "y2",
                         subsets = "group1"),
               "Subsets must be", fixed = TRUE)
})

