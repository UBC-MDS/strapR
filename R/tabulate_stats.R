#' Makes two presentation-ready tables that summarize the statistics from the bootstrapped
#' samples and the parameters for creating the bootstrapped samples.
#'
#' A bootstrapped confidence interval for the desired estimator for
#' the provided sample is calculated for a confidence level `level`.
#' Other stats and parameters of the distribution and sample are
#' also returned.
#'
#' @param stat_list A named list of the summary statistics produced
#'             by the `calculate_boot_stats` function
#' @param precision A integer value for the precision of the
#'                  table values
#' @param path A character vector with the path to where the tables are
#'                     to be saved to
#'
#' @return a list containing 2 tibble objects:  table 1- summary statistics
#'          table 2- bootstrapping parameters
#' @export
#'
#' @examples
#' st <- calculate_boot_stats(c(1, 2, 3, 4), 1000, level = 0.95, seed = 123)
#' result  <-  tabulate_stats(st)
#' result[[1]] # stats table
#' result[[2]] # parameter table
tabulate_stats <- function(stat_list, precision = 2, path = NULL) {

  # Declaring variables and functions to address check() notes
  dist <- NULL
  where <- tidyselect::vars_select_helpers$where
  lower <- stat_list["lower"]
  upper <- stat_list["upper"]
  std_err <- stat_list["std_err"]
  sample_size <- stat_list["sample_size"]
  level <- stat_list["level"]
  n <- stat_list["n"]

  if ("dist" %in% names(stat_list)) {
    dist <- stat_list["dist"]
    stat_list <- within(stat_list, rm(dist))
  }

  summary <- dplyr::as_tibble(stat_list)

  if(!is.character(path) & !is.null(path)) {
    stop("path the folder should be a character vector")
  }

  if(precision%%1 != 0 | precision < 0) {
    stop("Precision paramter should be a positive integer")
  }


  if (!(c("estimator") %in% colnames(summary))){
    stop("stat_list needs to be list outputted from the
         calculate_boot_stats() function")
  }

  if (!is.null(path)){
    if(dir.exists(path) == FALSE){
      stop("The path you want to save your tables to doesn't exist")
    }
  }

  estimator <- paste0("sample_", summary$estimator)
  name_check <- lapply(names(summary),
         "%in%",
         c("lower","upper","std_err", "level","sample_size","n",
           "rep","estimator", estimator))


  if(all(name_check =! TRUE)){
    stop("stat_list paramter needs to be  the list
         outputted from the calculate_boot_stats() function")
  }

  stat_summary <- summary |>
    dplyr::select({{ estimator }}, lower, upper, std_err) |>
    dplyr::mutate(dplyr::across(where(is.numeric), round, precision)) |>
    dplyr::rename("Lower Bound CI" = lower,
           "Upper Bound CI" = upper,
           "Standard Error" = std_err)

  if (summary$estimator == "mean"){
    stat_summary <- stat_summary |>
      dplyr::rename("Sample Mean" = {{ estimator }})
  } else if (summary$estimator == "median") {
    stat_summary <- stat_summary |>
      dplyr::rename("Sample Median" = {{ estimator}})
  } else if (summary$estimator == "var") {
    stat_summary <- stat_summary |>
      dplyr::rename("Sample Variance" = {{ estimator }})
  } else if (summary$estimator == "sd") {
    stat_summary <- stat_summary |>
      dplyr::rename("Sample Standard Deviation" = {{ estimator }})
  }

  if (summary$n == "auto"){
    ss <-  summary$sample_size
  } else {
    ss <- summary$n
  }

  if (!is.null(path)){
    caption <- paste0("Bootstrapping sample statistics from sample with ",
                    ss," records")
    stat_summary |>
      knitr::kable(output = FALSE, caption = caption) |>
      kableExtra::kable_styling() |>
      kableExtra::as_image(file = paste0(path, "Sampling_Statistics.png"))
  }

  if (summary$n !=  "auto"){
    bootstrap_summary <-  summary |>
      dplyr::select(sample_size, rep, level, n) |>
      dplyr::mutate(level = round(1-level, 3)) |>
      dplyr::rename("Sample Size" = sample_size,
             "Repetitons" = rep,
             "Significance Level" = level,
             "Samples per Bootstrap" = n)
  } else {
    bootstrap_summary <-  summary |>
      dplyr::select(sample_size, rep, level) |>
      dplyr::mutate(level = round(1-level, 3)) |>
      dplyr::rename("Sample Size" = sample_size,
             "Repetitons" = rep,
             "Significance Level" = level)
  }

  if (!is.null(path)){
    caption <- paste0("Bootstrapping Parameters ")
    bootstrap_summary |>
      knitr::kable(output = FALSE, caption = caption) |>
      kableExtra::kable_styling() |>
      kableExtra::as_image(file = paste0(path, "Bootsrapping_table.png"))
  }

  return(list(stat_summary, bootstrap_summary))
}









