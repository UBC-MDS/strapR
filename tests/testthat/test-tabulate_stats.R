calc_stats <- calculate_boot_stats(c(1:15),
                                   1000,
                                   level = 0.95,
                                   seed = 1)

calc_stats_sd <- calculate_boot_stats(c(1:15),
                                     1000,
                                     level = 0.95,
                                     seed = 1,
                                     estimator = "sd")

calc_stats_var <- calculate_boot_stats(c(1:15),
                                     1000,
                                     level = 0.95,
                                     seed = 1,
                                     estimator = "var")

calc_stats_median <- calculate_boot_stats(c(1:15),
                                      1000,
                                      level = 0.95,
                                      seed = 1,
                                      estimator = "median")

st_withdist <- calculate_boot_stats(c(1:15), 1000, level = 0.95,
                                   seed = 1, pass_dist = TRUE)

test_that("Check if error thrown for invalid statistic input", {
  expect_error(tabulate_stats(list("g" = 1, "m" = 2)))
})

col_subset <- dplyr::as_tibble(calc_stats) |>
  dplyr::select(lower, upper, std_err, level, sample_size,
                n, rep, estimator)
test_that("Check if error thrown for invalid statistic input", {
  expect_error(tabulate_stats(col_subset))
})

test_that("Check if error thrown for negative precision input", {
  expect_error(tabulate_stats(calc_stats, precision = -1))
})

test_that("Check if error thrown for non-whole number precision input", {
  expect_error(tabulate_stats(calc_stats, precision = 10.90))
})

test_that("Check if distribution is dropped if present in statistics list", {
  expect_equal(tabulate_stats(calc_stats), tabulate_stats(st_withdist))
})

final_tables <- tabulate_stats(calc_stats)
stats <- final_tables[[1]]
params <- final_tables[[2]]

test_that("Check if handeling when n ='auto' correctly" , {
  expect_equal(params[[1, "Sample Size"]], calc_stats$sample_size)
  expect_equal(params[[1, "Sample Size"]], calc_stats_sd$sample_size)
  expect_equal(params[[1, "Sample Size"]], calc_stats_var$sample_size)
  expect_equal(params[[1, "Sample Size"]], calc_stats_median$sample_size)
})

st_change_n <- calculate_boot_stats(c(1:15), 1000, level = 0.95,
                                   n = 9, seed = 1)
final_tables <- tabulate_stats(st_change_n)
stats <- final_tables[[1]]
params <- final_tables[[2]]
test_that("Check if handeling when n ='auto' correctly" , {
  expect_equal(params[[1, "Samples per Bootstrap"]], st_change_n$n)
})

folder <- "no_folder/"
test_that("Check if error thrown if save folder misspecified", {
  expect_error(tabulate_stats(calc_stats, path = folder))
  expect_error(tabulate_stats(calc_stats, path = 123))
})

final_tables <- tabulate_stats(calc_stats_var)
test_that("Check for variance columns in statistic table", {
  expect_equal(colnames(final_tables[[1]]), c("Sample Variance",
                                              "Lower Bound CI",
                                              "Upper Bound CI",
                                              "Standard Error"))
})

final_tables <- tabulate_stats(calc_stats_median)
test_that("Check for variance columns in statistic table", {
  expect_equal(colnames(final_tables[[1]]), c("Sample Median",
                                              "Lower Bound CI",
                                              "Upper Bound CI",
                                              "Standard Error"))
})


final_tables <- tabulate_stats(calc_stats_sd)
test_that("Check for variance columns in statistic table", {
  expect_equal(colnames(final_tables[[1]]), c("Sample Standard Deviation",
                                              "Lower Bound CI",
                                              "Upper Bound CI",
                                              "Standard Error"))
})

test_that("Check that outputs are in a list", {
  expect_equal(is(final_tables,"list"), TRUE)
})

input_path <- "./"
file1 <- "./Bootsrapping_table.png"
file2 <- "./Sampling_Statistics.png"
tabulate_stats(calc_stats, path = input_path)
test_that("Check that the files are saved", {
  expect_equal(file.exists(file1), TRUE)
  expect_equal(file.exists(file2), TRUE)
})




