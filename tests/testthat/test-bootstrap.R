sample_1 <- c(1, 1, 1)
sample_2 <- c(1, 2, 3)

test_that("Check if error thrown for invalid sample input", {
  expect_error(bootstrap(c("a", "b"), 3, 3))
})

test_that("Check if error thrown for invalid rep input", {
  expect_error(bootstrap(c(1, 2, 3), "a", 3))
  expect_error(bootstrap(c(1, 2, 3), 0, 3))
})

test_that("Check if error thrown for invalid n input", {
  expect_error(bootstrap(c(1, 2, 3), 3, "catch me"))
  expect_error(bootstrap(c(1, 2, 3), 3, TRUE))
  expect_error(bootstrap(c(1, 2, 3), 3, 0))
})

test_that("Check if error thrown for invalid seed input", {
  expect_error(bootstrap(c(1, 2, 3), 3, 3, seed = -1))
  expect_error(bootstrap(c(1, 2, 3), 3, 3, seed = "two"))
})

test_that("Check if error thrown for invalid estimator input", {
  expect_error(bootstrap(c(1, 2, 3), 3, estimator = "mean"))
})

test_that("Check if bootstrap distribution is correct", {
  expect_equal(bootstrap(sample_1, 3, 3), c(1, 1, 1))
  expect_equal(bootstrap(sample_1, 3, 3, estimator = median), c(1, 1, 1))
  expect_equal(bootstrap(sample_1, 3, 3, estimator = sd), c(0, 0, 0))
  expect_equal(bootstrap(sample_1, 3, 3, estimator = var), c(0, 0, 0))
})

test_that("Check if random seed works", {
  expect_equal(bootstrap(sample_2, 3, 3, estimator = median, seed = 1),
               c(1, 2, 2))
  expect_equal(bootstrap(sample_2, 3, 3, estimator = median, seed = 1),
               c(1, 2, 2))
})
