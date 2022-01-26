#' Bootstraps a sampling distribution for a sample.
#'
#' A sampling distribution of `rep` replicates is generated
#' for the specified `estimator` with replacement with a
#' bootstrap sample size of `n`.
#'
#' @param sample A numeric vector to bootstrap
#' @param rep A numeric vector for number of replicates
#' @param n A numeric or character vector for the size of bootstrap samples
#' @param estimator A function that is one of `mean`, `median`, `var`, `sd`
#' @param seed A numeric vector as random seed. Can be `NULL`
#'
#' @return A numeric vector for bootstrap distribution
#' @export
#'
#' @examples
#' bootstrap(c(1, 2, 3), 3, 3)
#' bootstrap(c(1, 2, 3), 3, 3, estimator = var, seed = 1)
bootstrap <- function(sample, rep, n = "auto", estimator = mean,
                      seed = NULL) {
  if(!is.numeric(sample)) {
    stop("sample should be a numeric vector")
  }

  if(!is.numeric(rep)) {
    stop("rep should be of type 'numeric'")
  }

  if(rep < 1) {
    stop("Invalid value for rep, should be greater than 0")
  }

  if(!(is.character(n) || is.numeric(n))) {
    stop("n should be of type 'character' or 'numeric'")
  }

  if(is.character(n) && n != "auto") {
    stop("Invalid value for n. Did you intend n='auto'?")
  }

  if(is.numeric(n) && n < 1) {
    stop("Invalid value for n, should be greater than 0")
  }

  if(!is.function(estimator)) {
    stop("estimator should be of type 'function'")
  }

  if(!(as.character(substitute(estimator))
       %in% c("mean", "median", "var", "sd"))) {
    stop("Supported estimators are mean, median, var, sd")
  }

  if(!(is.null(seed) || is.numeric(seed))) {
    stop("seed should be NULL or of type 'numeric'")
  }

  if(is.numeric(seed) && seed < 1) {
    stop("Invalid value for seed, should be greater than 0")
  }

  if(n == "auto") {
    n = length(sample)
  }

  if(!is.null(seed)) {
    seed <- round(seed)
    set.seed(seed)
  }

  n <- round(n)
  rep <- round(rep)

  replicates <- lapply(1:rep, function(i) sample(sample, size = n, replace = T))

  sapply(replicates, estimator)
}
