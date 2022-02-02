#' Calculates a bootstrapped confidence interval and other stats for a sample.
#'
#' A bootstrapped confidence interval for the desired estimator for
#' the provided sample is calculated for a confidence level `level`.
#' Other stats and parameters of the distribution and sample are
#' also returned.
#'
#' @param sample A numeric vector to bootstrap
#' @param rep A integer vector for number of replicates
#' @param n A integer or character vector for the size of bootstrap samples
#' @param level A numeric vector for the confidence level
#' @param estimator A character vector containing one of the
#'                  ("mean", "median", "var", "sd") estimators
#' @param seed A integer vector as random seed. Can be \code{NULL}
#' @param pass_dist A boolean vector to decide whether to return the
#'                  bootstrapped distribution
#'
#' @return A named list
#' @export
#'
#' @examples
#' calculate_boot_stats(c(1, 2, 3, 4), 1000, level = 0.95, seed = 1)
#' calculate_boot_stats(c(1, 2, 3, 4), 1000, level = 0.95, estimator = "median",
#' seed = 1, pass_dist = TRUE)

calculate_boot_stats <- function(sample, rep, n = "auto", level = 0.95,
                            estimator = "mean", seed = NULL,
                            pass_dist = FALSE) {

  if(!is.numeric(level)) {
    stop("level should be a numeric value")
  }

  if(!length(level) == 1) {
    stop("level should only be length 1")
  }

  if(!(level > 0 && level < 1 )) {
    stop("level should be between 0 and 1")
  }

  if(!is.logical(pass_dist)) {
    stop("pass_dist should be logical (TRUE or FALSE)")
  }

  if(!is.character(estimator)) {
    stop("Estimator should be a string")
  }

  if(!(estimator %in% c("mean", "median", "var", "sd"))) {
    stop("Supported estimators are mean, median, var, sd")
  }

  if(level < 0.7) {
    warning("Warning: chosen level is quite low--level is a confidence level,
            not a signficance level")
  }

  # Declaring variables to avoid notes in check()
  median <- median
  sd <- sd
  quantile <- quantile
  var <- var
    
  estimator_list <- list(
    "mean" = mean,
    "median" = median,
    "var" = var,
    "sd" = sd
  )

# get the bootstrapped mean vector
  dist <- bootstrap(sample = sample,
                   rep = rep,
                   n = n,
                   estimator = estimator_list[[estimator]],
                   seed = seed)

  stats_list <- list(
    "lower" = quantile(dist, probs = (1-level)/2),
    "upper" = quantile(dist, probs = (1 - ( (1-level)/2) )),
    "sample_estimate" = estimator_list[[estimator]](sample),
    "std_err" = sd(dist),
    "level" = level,
    "sample_size" = length(sample),
    "n" = n,
    "rep" = rep,
    "estimator" = estimator
  )

  # rename the sample estimator dynamically
    # make sure the order of the list is correct before renaming
    if (!(names(stats_list)[3] == "sample_estimate")) {
      stop("third index of stats list must be the sample estimate")
    }

    # rename
    names(stats_list)[[3]] <- paste0("sample_", estimator)


  if (pass_dist == TRUE) {

    stats_list["dist"] <- list(dist)
    return(stats_list)

  } else {

    return(stats_list)

  }

}
