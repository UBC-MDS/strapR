#' Bootstraps a sampling distribution for a sample.
#'
#' A sampling distribution of \code{rep} replicates is generated
#' for the specified \code{estimator} with replacement with a
#' bootstrap sample size of \code{n}.
#'
#' @param sample A numeric vector to bootstrap
#' @param rep A integer vector for number of replicates
#' @param n A integer or character vector for the size of bootstrap samples
#' @param estimator A character vector containing one of the
#'                  ("mean", "median", "var", "sd") estimators
#' @param seed A integer vector as random seed. Can be \code{NULL}
#'
#' @return A numeric vector for bootstrap distribution
#' @export
#'
#' @examples
#' bootstrap(c(1, 2, 3), 3, 3)
#' bootstrap(c(1, 2, 3), 3, 3, estimator = "var", seed = 1)
bootstrap <- function(sample, rep, n = "auto", estimator = "mean",
                                   seed = NULL) {
}
