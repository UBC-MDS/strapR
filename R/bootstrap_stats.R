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
#' calculate_boot_stats(c(1, 2, 3, 4), 1000, 1000, level = 0.95, seed = 1)

calculate_boot_stats <- function(sample, rep, n = "auto", level = 0.95,
                            estimator = mean, seed = NULL,
                            pass_dist = FALSE) {
  source("bootstrap.R")

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
  
  if(level < 0.7) {
    warning("Warning: chosen level is quite low--level is a confidence level,
            not a signficance level")
  }
  
  # get the bootstrapped mean vector
  dist = bootstrap(sample=sample,
                   rep=rep,
                   n=n,
                   estimator=mean,
                   seed=seed)
  
  stats_list = list(
    "test" = dist,
    paste("sample_", as.character(substitute(estimator)), sep = "") = 2
  )
  
}

x = calculate_boot_stats(c(1, 2, 3, 4), rep = 20)