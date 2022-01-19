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
bootstrap_stats <- function(sample, rep, n = "auto", level = 0.95,
                            estimator = "mean", seed = NULL,
                            pass_dist = FALSE) {
}
