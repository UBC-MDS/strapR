test_that("Check if error thrown for invalid title input", {
  expect_error(plot_ci(c(1, 2, 3), 1000,
                       n = 100, 
                       ci_level = 0.95, 
                       ci_random_seed = 123, 
                       title = 123))
})

test_that("Check if error thrown for invalid x_axis input", {
  expect_error(plot_ci(c(1, 2, 3), 1000,
                       n = 100, 
                       ci_level = 0.95, 
                       ci_random_seed = 123, 
                       x_axis = 123))
})

test_that("Check if error thrown for invalid y_axis input", {
  expect_error(plot_ci(c(1, 2, 3), 1000,
                       n=100, 
                       ci_level = 0.95, 
                       ci_random_seed = 123, 
                       y_axis = 123))
})

test_that("Check if error thrown for invalid save_result_to input", {
  expect_error(plot_ci(c(1, 2, 3), 1000,
                       n=100, 
                       ci_level = 0.95, 
                       ci_random_seed = 123, 
                       save_result_to = 123))
})


test_that("Check the output is a ggplot object", {
  expect_equal(is(plot_ci(c(1, 2, 3), 1000),
                  "ggplot"),
               TRUE)
})
