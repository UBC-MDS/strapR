# integration

test_that("check bootstrap returns right type", {
  expect_equal(is.numeric(bootstrap(c(1, 2, 3), 100)),
               TRUE)
})

# set up test outputs

test_list_1 <- calculate_boot_stats(c(1, 2, 3), 100, seed = 12)


test_list_2 = calculate_boot_stats(
  c(1000, 2000, 3000, 4000),
  n=4,
  rep=1000,
  level=0.9,
  seed=1234,
  estimator='var')

test_list_3 = calculate_boot_stats(
  c(1000, 2000, 3000, 4000),
  n=5,
  rep=1000,
  level=0.9,
  seed=1234,
  estimator='var')

# check output is expected

test_that("check calculate boot stats returns right values", {
  expect_equal(round(test_list_1$lower[[1]], 7),
               1.3333333)
  expect_equal(round(test_list_1$upper[[1]], 7),
               2.6666667)
  expect_equal((test_list_1[[3]][[1]]),
               2)
  expect_equal(round(test_list_1$std_err[[1]], 7),
               0.3949598)
  expect_equal((test_list_1$level[[1]]),
               0.95)
  expect_equal((test_list_1$n[[1]]),
               "auto")
  expect_equal((test_list_1$rep[[1]]),
               100)
  expect_equal((test_list_1$estimator[[1]]),
               "mean")
  
})


test_that("check that increasing sampling number decreases standard error", {
  expect_equal(test_list_2$std_err[[1]] > test_list_3$std_err[[1]],
               TRUE)
})

test_that("check that changing parameters changes values", {
  expect_equal(test_list_1$level[[1]] > test_list_2$level[[1]], TRUE)
  expect_equal(test_list_3$estimator[[1]], "var")
})

# test errors

test_that("check level error is throwing properly", {
  expect_error(calculate_boot_stats(c(1, 2, 3), 100, level=1))
  expect_error(calculate_boot_stats(c(1, 2, 3), 100, level=0))
  expect_error(calculate_boot_stats(c(1, 2, 3), 100, level="Over 9000"))
})

test_that('check estimator error throwing properly', {
  expect_error(calculate_boot_stats(c(1, 2, 3), 100, estimator = mean))
  expect_error(calculate_boot_stats(c(1, 2, 3), 100, estimator = "max"))
})

test_that('check that pass_dist error throws properly', {
  expect_error(calculate_boot_stats(c(1, 2, 3), 100, pass_dist = "Yes"))
  expect_error(calculate_boot_stats(c(1, 2, 3), 100, pass_dist = 1))
})