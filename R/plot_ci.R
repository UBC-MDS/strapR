#' Plots a bootstrapped sampling distribution with its confidence 
#' interval and observed mean.
#'
#' A bootstrapped confidence interval for the desired estimator for
#' the provided sample is calculated for a confidence level `level`.
#' Other stats and parameters of the distribution and sample are
#' also returned.
#'
#' @param sample A numeric vector to bootstrap
#' @param rep A integer vector for number of replicates
#' @param bin_size A integer vector for number of bins representing 
#'                 intervals of equal size over the range
#' @param n A integer or character vector for the size of bootstrap 
#'          samples
#' @param ci_level A numeric vector for the confidence level
#' @param ci_random_seed A integer vector as random seed. 
#' @param title A character vector for the title of the histogram
#' @param x_axis A character vector for the name of the x axis
#' @param y_axis A character vector for the name of the y axis
#' @param save_result_to A character vector for the path to directory 
#'                       from current directory to save plot
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' plot_ci(c(1, 2, 3, 4, 5, 6, 7), 1000 ,n=100, 
#'                   ci_level = 0.95, ci_random_seed = 123)
#' plot_ci(c(1, 2, 3, 4, 5, 6, 7), 1000 ,n=100, 
#'                  ci_level = 0.95, ci_random_seed = 123, 
#'                  save_result_to = "../")
plot_ci <- function(sample, rep, bin_size = 30, 
                              n = "auto",ci_level = 0.95, 
                              ci_random_seed = NULL, 
                              title = "Bootstrap Sampling Plot", 
                              x_axis = "Bootstrap Sample Mean",
                              y_axis = "Count", 
                              save_result_to = ".") {
  
  if(!is.character(title)) {
    stop("title should be a character vector")
  }
  
  if(!is.character(x_axis)) {
    stop("x_axis should be a character vector")
  }
  
  if(!is.character(y_axis)) {
    stop("y_axis should be a character vector")
  }
  
  if(!is.character(save_result_to)) {
    stop("save_result_to should be a character vector")
  }
  
  # access the calculate_boot_stats function
  source("calculate_boot_stats.R")
  
  sample_stat_dict <- calculate_boot_stats(sample, 
                                          rep, 
                                          level=ci_level, 
                                          seed = ci_random_seed, 
                                          pass_dist=TRUE)
  
  bootstrap_dist <- data.frame(matrix(unlist(sample_stat_dict$dist), 
                                      nrow=length(sample_stat_dict$dist), 
                                      byrow=TRUE))
  
  colnames(bootstrap_dist) <- c("sample_m")
  
  bootstrap_dist_ci <- ggplot(bootstrap_dist, aes(sample_m)) +
    geom_histogram(fill = "dodgerblue3", 
                   color = "lightgrey", 
                   bins = bin_size) +
    labs(x = x_axis, 
         y = y_axis) +
    theme(text = element_text(size = 16.5)) +
    geom_vline(xintercept = sample_stat_dict$sample_mean, 
               colour = "red", 
               size = 1) + 
    geom_vline(xintercept = unname(sample_stat_dict$lower), 
               colour = "purple", 
               size = 1, 
               linetype = "dashed") +  
    geom_vline(xintercept = unname(sample_stat_dict$upper), 
               colour = "purple", 
               size = 1, 
               linetype = "dashed")
  
  str_1 <- toString(round(sample_stat_dict$sample_mean, 2))
  str_2 <- toString(round(sample_stat_dict$std_err, 2))
  
  annotated_plot <- bootstrap_dist_ci +
    annotate(geom="label", 
             x = sample_stat_dict$sample_mean, 
             y = 0.9 * ggplot_build(bootstrap_dist_ci)$layout$panel_params[[1]]$y.range[2],   
             label = paste(str_1, "+/-", str_2), 
             size = 3, 
             color = "red",
             fill=" lightgrey") + 
    annotate(geom="label", 
             x = unname(sample_stat_dict$lower), 
             y = 0.9 * ggplot_build(bootstrap_dist_ci)$layout$panel_params[[1]]$y.range[2], 
             label = (toString(round(unname(sample_stat_dict$lower), 2))), 
             size = 3, 
             color = "purple",
             fill=" lightgrey") +
    annotate(geom="label", 
             x = unname(sample_stat_dict$upper), 
             y = 0.9 * ggplot_build(bootstrap_dist_ci)$layout$panel_params[[1]]$y.range[2], 
             label = (toString(round(unname(sample_stat_dict$upper), 2))), 
             size = 3, 
             color = "purple",
             fill=" lightgrey") +
    ggtitle(title)
  
  ggsave(filename = paste(title, ".png"), 
         plot = annotated_plot, 
         path = save_result_to)
  
  return(annotated_plot)
}


