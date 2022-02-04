#' Plots a bootstrapped sampling distribution with its confidence interval and
#' observed mean.
#'
#' A bootstrapped confidence interval for the desired estimator for the provided
#' sample is calculated for a confidence level `level`.
#'
#' Other stats and parameters of the distribution and sample are
#' also returned.
#'
#' @param sample A numeric vector to bootstrap
#' @param rep A integer vector for number of replicates
#' @param bin_size A integer vector for number of bins representing intervals of
#'                 equal size over the range
#' @param n A integer or character vector for the size of bootstrap samples
#' @param level A numeric vector for the confidence level
#' @param seed A integer vector as seed.
#' @param title A character vector for the title of the histogram
#' @param y_axis A character vector for the name of the y axis
#' @param estimator A character vector containing one of the("mean", "median",
#'                  "var", "sd") estimators
#' @param path A character vector for the path to directory from
#'             current directory to save plot
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' plot_ci(c(1, 2, 3, 4, 5, 6, 7), 1000, n = 100, level = 0.95)
#' plot_ci(c(1, 2, 3, 4, 5, 6, 7), 1000, n = 100, path = "../")

plot_ci <- function(sample, rep, bin_size = 30, n = "auto", level = 0.95,
                    seed = NULL, title = "", y_axis = "Count",
                    estimator = "mean", path = NULL) {

  if(!is.character(title)) {
    stop("title should be a character vector")
  }

  if(!is.character(y_axis)) {
    stop("y_axis should be a character vector")
  }

  if (!is.null(path)){
    if(!is.character(path)) {
    stop("save_result_to should be a character vector")
    }
  }

  if (!is.null(path)){
    if(dir.exists(path) == FALSE){
      stop("The path you want to save your tables to doesn't exist")
    }
  }

  sample_stat_dict <- calculate_boot_stats(sample, rep, level = level,
                                           seed = seed,
                                           estimator = estimator,
                                           pass_dist = TRUE)

  bootstrap_dist <- data.frame(matrix(unlist(sample_stat_dict$dist),
                                      nrow = length(sample_stat_dict$dist),
                                      byrow = TRUE))

  est_v <- sample_stat_dict[[paste("sample_", estimator, sep = '')]]

  value <- names(bootstrap_dist)[1]

  bootstrap_dist_ci <- ggplot2::ggplot(bootstrap_dist,
                                       ggplot2::aes_string(x = value)) +
    ggplot2::geom_histogram(fill = "dodgerblue3", color = "lightgrey",
                            bins = bin_size) +
    ggplot2::labs(x = paste("Bootstrap sample ", estimator, sep = ''),
                  y = y_axis) +
    ggplot2::theme(text = ggplot2::element_text(size = 16.5)) +
    ggplot2::geom_vline(xintercept = est_v, colour = "red", size = 1) +
    ggplot2::geom_vline(xintercept = unname(sample_stat_dict$lower),
                        colour = "purple", size = 1, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = unname(sample_stat_dict$upper),
                        colour = "purple", size = 1, linetype = "dashed")

  str_1 <- toString(round(est_v, 2))

  str_2 <- toString(round(sample_stat_dict$std_err, 2))

  y_panel <- ggplot2::ggplot_build(bootstrap_dist_ci)$layout$panel_params[[1]]

  y_max <- y_panel$y.range[2]

  lower_label <- toString(round(unname(sample_stat_dict$lower), 2))

  upper_label <- toString(round(unname(sample_stat_dict$upper), 2))

  annotated_plot <- bootstrap_dist_ci +
    ggplot2::annotate(geom ="label", x = est_v, y = 0.9 * y_max,
                      label = paste(str_1, "+/-", str_2), size = 3,
                      color = "red", fill = " lightgrey") +
    ggplot2::annotate(geom ="label",  x = unname(sample_stat_dict$lower),
                      y = 0.9 * y_max, label = lower_label, size = 3,
                      color = "purple", fill = " lightgrey") +
    ggplot2::annotate(geom ="label", x = unname(sample_stat_dict$upper),
                      y = 0.9 * y_max, label = upper_label, size = 3,
                      color = "purple", fill = " lightgrey") +
    ggplot2::ggtitle(title)

  if (!is.null(path)){
    ggplot2::ggsave(filename = paste(path,"Bootstrap_Histogram.png",sep = ''), 
                    plot = annotated_plot, path = path)
  }

  annotated_plot
}


