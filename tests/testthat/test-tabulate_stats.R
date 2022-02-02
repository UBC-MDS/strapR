calc_stats = calculate_boot_stats(c(1:15), 1000, level = 0.95, seed = 1)
st_withdist = calculate_boot_stats(c(1:15), 1000, level = 0.95,
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

test_that("Check if error thrown for non-boolean save input", {
  expect_error(tabulate_stats(calc_stats, save = "yes"))
})

test_that("Check if distribution is dropped if present in statistics list", {
  expect_equal(tabulate_stats(calc_stats), tabulate_stats(st_withdist))
})

final_tables <- tabulate_stats(calc_stats)
stats <- final_tables[[1]]
params <- final_tables[[2]]

test_that("Check if handeling when n ='auto' correctly" , {
  expect_equal(params[[1, "Sample Size"]], calc_stats$sample_size)
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
test_that("Check if error thrown if save folder doesn't exist", {
  expect_error(tabulate_stats(calc_stats, folder_path = folder))
})
