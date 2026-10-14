# Shared objects for the tests. testthat sources helper files before the test
# files, so the specifications are fitted once, outside the individual tests.

# Two independent variables, one dependent variable, one control (test-specr.R)
specs_simple <- setup(data = example_data,
                      x = c("x1", "x2"),
                      y = "y1",
                      model = "lm",
                      controls = "c1")

# With subsets and two controls (test-generics.R)
specs_subsets <- setup(data = example_data,
                       x = c("x1", "x2"),
                       y = "y1",
                       model = "lm",
                       subsets = list(group1 = unique(example_data$group1)),
                       controls = c("c1", "c2"))
results_subsets <- specr(specs_subsets)

# Two dependent variables and two controls (test-infer_curve.R)
specs_infer <- setup(data = example_data,
                     x = c("x1", "x2"),
                     y = c("y1", "y2"),
                     model = "lm",
                     controls = c("c1", "c2"))
results_infer <- specr(specs_infer)

set.seed(1)
inference <- infer_curve(results_infer, specs_infer, n_samples = 50)
