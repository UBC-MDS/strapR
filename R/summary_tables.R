#' Makes two tables that summerize the statistics from the bootstrapped 
#' samples and the parameters for creating the bootstrapped samples.
#'
#' A bootstrapped confidence interval for the desired estimator for
#' the provided sample is calculated for a confidence level `level`.
#' Other stats and parameters of the distribution and sample are
#' also returned.
#'
#' @param stat A named list to for summary statistics produced 
#'             by the `bootstrap_stats` function
#' @param precision A integer vector for the precision of the table values
#' @param estimator A logical vector to include the bootstrap estimate 
#'                  in the summary statistics table
#' @param alpha A logical vector to include the significance level 
 #'             in the summary statistics table
#'
#' @return table objects
#' @export
#'
#' @examples
#' st <- bootstrap_stats([1, 2, 3, 4], 1000, level=0.95, random_seed=123)
#' stats_table, parameter_table  <-  summary_tables(st)
#' stats_table
#' parameter_table
summary_tables <- function(stat, precision=2, estimator=TRUE, alpha=TRUE) {
}