test_that("the example data have the documented structure", {
  expect_s3_class(example_data, "tbl_df")
  expect_equal(dim(example_data), c(1000, 15))
  expect_named(example_data, c("group1", "group2", "group3",
                               paste0("x", 1:4), paste0("c", 1:4), paste0("y", 1:4)))
  expect_true(all(purrr::map_lgl(example_data[c(paste0("x", 1:4), paste0("y", 1:4), paste0("c", 1:4))], is.numeric)))
  expect_true(all(purrr::map_lgl(example_data[c("group1", "group2", "group3")], is.character)))
  expect_false(anyNA(example_data))
})
