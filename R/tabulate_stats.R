#' Makes two presentation-ready tables that summarize the statistics from the bootstrapped
#' samples and the parameters for creating the bootstrapped samples.
#'
#' A bootstrapped confidence interval for the desired estimator for
#' the provided sample is calculated for a confidence level `level`.
#' Other stats and parameters of the distribution and sample are
#' also returned.
#'
#' @param stat_list A named list of the summary statistics produced
#'             by the `bootstrap_stats` function
#' @param precision A integer value for the precision of the
#'                  table values
#' @param estimator A logical indicating if we are to include the bootstrap
#'                  estimate in the summary statistics table
#' @param save  A logical indicating if tables should  be saved as a png to disk
#'
#'  @param folder_path A character vector with the path to where the tables are
#'                     to be saved to
#'
#' @return a list containing 2 table objects:  table 1- summary statistics
#'          table 2- bootstraping parameters
#' @export
#'
#' @examples
#' st <- bootstrap_stats([1, 2, 3, 4], 1000, level=0.95,
#'                       random_seed=123)
#' result  <-  tabulate_stats(st)
#' result[1] # stats table
#' result[2] # parameter table
tabulate_stats <- function(stat_list, precision=2, estimator=TRUE,
                           save = FALSE, folder_path="") {

  if(!is.character(folder_path)) {
    stop("path the folder should be a character vector")
  }


  if(!is.integer(precision) | preision <0) {
    stop("Precision paramter should be a positive integer")
  }

  if(!is.logical(estimator) | !is.logical(alpha) | !is.logical(save)) {
    stop("The estimator, alpha and save parameters should be TRUE or FALSE")
  }

  if (is.null(summary$estimator)==TRUE){
    stop("stat_list needs to be list outputted from the bootstrap_stats() function")
  }

  esti <- paste0("sample_", summary$estimator)
  name_check <- lapply(names(summary),
         "%in%",
         c("lower","upper","std_err", "level","sample_size","n","rep","estimator", esti))


  if(all(name_check=!TRUE)){
    stop("stat_list paramter needs to be  the list
         outputted from the bootstrap_stats() function")
  }

  summary <- as_tibble(stat_list)

  if (estimator == TRUE){
    esti <- paste0("sample_",summary$estimator)
    Name <- paste0("Sample ",summary$estimator)
    stat_summary <- summary |>
      select(lower,upper, std_err, {{ esti }}) |>
      mutate(across(where(is.numeric), round, precision)) |>
      rename("Lower Bound CI"=lower,
             "Upper Bound CI" = upper,
             "Standard Error"=std_err)

    if (summary$estimator=="mean"){
      stat_summary <- stat_summary |>
        rename("Sample Mean" = {{ esti }})
    } else if (summary$estimator=="median") {
      stat_summary <- stat_summary |>
        rename("Sample Median" = {{ esti }})
    } else if (summary$estimator=="var") {
      stat_summary <- stat_summary |>
        rename("Sample Variance" = {{ esti }})
    } else if (summary$estimator=="sd") {
      stat_summary <- stat_summary |>
        rename("Sample Standard Deviation" = {{ esti }})
    }

  }

  caption= paste0("Bootstrapping sample statistics from sample with ",
                  summary$n," records")
  st_table <- knitr::kable(stat_summary, output = FALSE,
                           caption = caption) |>
    kable_paper("hover", full_width = F)

  if (save == TRUE){
    save_kable(st_table, file = paste0(folder_path,"Sampling_Statistics.png"))
  }


  if (summary$n !=  "auto"){
    bootstrap_summary =  summary |>
      select(sample_size, rep,level,n) |>
      mutate(level = round(1-level,3)) |>
      rename("Sample Size"=sample_size,
             "Repetitons"=rep,
             "Significance Level"=level,
             "Samples per Bootstrap"= n)
  } else {
    bootstrap_summary =  summary |>
      select(sample_size, rep,level) |>
      mutate(level = round(1-level,3)) |>
      rename("Sample Size"=sample_size,
             "Repetitons"=rep,
             "Significance Level"=level)
  }

  caption= paste0("Bootstrapping Parameters ")
  bs_table <- knitr::kable(bootstrap_summary, output = FALSE,
                           caption = caption) |>
    kable_paper("hover", full_width = F)

  if (save == TRUE){
    save_kable(bs_table, file = paste0(folder_path,"Bootstrap_parameter_table.png"))
  }

  return(list(st_table, bs_table))
}









