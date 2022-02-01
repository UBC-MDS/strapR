test_that("Check if error thrown for invalid title input", {
  expect_error(plot_ci(c(1, 2, 3), 1000, n = 100, title = 123))
})

test_that("Check if error thrown for invalid y_axis input", {
  expect_error(plot_ci(c(1, 2, 3), 1000, n = 100, y_axis = 123))
})

test_that("Check if error thrown for invalid path input", {
  expect_error(plot_ci(c(1, 2, 3), 1000, n = 100, path = 123))
})

test_that("Check if error thrown for invalid path input value", {
  expect_error(plot_ci(c(1, 2, 3), 1000, n = 100, path = "Users"))
})

test_that("Check the output is a ggplot object", {
  expect_equal(is(plot_ci(c(1, 2, 3), 1000),"ggplot"), TRUE)
})
