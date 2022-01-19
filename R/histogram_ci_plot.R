#' Plots a bootstrapped sampling distribution with its confidence interval and observed mean.
#'
#' Makes a histogram of a bootstrapped sampling distribution 
#' with its confidence interval and observed mean.
#'
#' @param sample A numeric vector to bootstrap
#' @param rep A integer vector for number of replicates
#' @param bin_size A integer vector for number of bins representing intervals of equal size over the range
#' @param n A integer or character vector for the size of bootstrap samples
#' @param ci_level A numeric vector for the confidence level
#' @param ci_random_seed A integer vector as random seed. 
#' @param title A character vector for the title of the histogram
#' @param x_axis A character vector for the name of the x axis
#' @param y_axis A character vector for the name of the y axis
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' histogram_ci_plot(c(1, 2, 3, 4, 5, 6, 7), 1000 ,n=100, ci_level = 0.95, ci_random_seed = 123)
histogram_ci_plot <- function(sample, rep, bin_size = 30, n = "auto", ci_level = 0.95, 
                              ci_random_seed = None, title = "", x_axis = "Bootstrap Sample Mean",
                              y_axis = "Count") {
}