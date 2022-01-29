#' Makes two presentation-ready tables that summarize the statistics from the bootstrapped
#' samples and the parameters for creating the bootstrapped samples.
#'
#' A bootstrapped confidence interval for the desired estimator for
#' the provided sample is calculated for a confidence level `level`.
#' Other stats and parameters of the distribution and sample are
#' also returned.
#'
#' @param stat_list A named list of the summary statistics produced
#'             by the `calculate_boot_stats` function
#' @param precision A integer value for the precision of the
#'                  table values
#' @param save  A logical indicating if tables should  be saved as a png to disk
#'
#' @param folder_path A character vector with the path to where the tables are
#'                     to be saved to
#'
#' @return a list containing 2 tibble objects:  table 1- summary statistics
#'          table 2- bootstrapping parameters
#' @export
#'
#' @examples
#' st <- calculate_boot_stats(c(1, 2, 3, 4), 1000, level=0.95,
#'                       seed=123)
#' result  <-  tabulate_stats(st)
#' result[1] # stats table
#' result[2] # parameter table
tabulate_stats <- function(stat_list, precision=2, save = FALSE, folder_path="") {

  if ("dist" %in% names(stat_list)) {
    stat_list = within(stat_list, rm(dist))
  }

  summary <- dplyr::as_tibble(stat_list)



  if(!is.character(folder_path)) {
    stop("path the folder should be a character vector")
  }


  if(precision%%1!=0 | precision < 0) {
    stop("Precision paramter should be a positive integer")
  }

  if(!is.logical(save)) {
    stop("The save parameters should be TRUE or FALSE")
  }

  if (is.null(summary$estimator)==TRUE){
    stop("stat_list needs to be list outputted from the calculate_boot_stats() function")
  }

  if (folder_path != ""){
    if(dir.exists(folder_path)==FALSE){
      stop("The path you want to save your tables to doesn't exist")
    }
  }

  esti <- paste0("sample_", summary$estimator)
  name_check <- lapply(names(summary),
         "%in%",
         c("lower","upper","std_err", "level","sample_size","n","rep","estimator", esti))


  if(all(name_check=!TRUE)){
    stop("stat_list paramter needs to be  the list
         outputted from the calculate_boot_stats() function")
  }

  esti <- paste0("sample_",summary$estimator)
  Name <- paste0("Sample ",summary$estimator)
  stat_summary <- summary |>
    dplyr::select({{ esti }},lower,upper, std_err) |>
    dplyr::mutate(dplyr::across(where(is.numeric), round, precision)) |>
    dplyr::rename("Lower Bound CI"=lower,
           "Upper Bound CI" = upper,
           "Standard Error"=std_err)

  if (summary$estimator=="mean"){
    stat_summary <- stat_summary |>
      dplyr::rename("Sample Mean" = {{ esti }})
  } else if (summary$estimator=="median") {
    stat_summary <- stat_summary |>
      dplyr::rename("Sample Median" = {{ esti }})
  } else if (summary$estimator=="var") {
    stat_summary <- stat_summary |>
      dplyr::rename("Sample Variance" = {{ esti }})
  } else if (summary$estimator=="sd") {
    stat_summary <- stat_summary |>
      dplyr::rename("Sample Standard Deviation" = {{ esti }})
  }

  if (summary$n=="auto"){
    ss = summary$sample_size
  } else {
    ss= summary$n
  }


  if (save == TRUE | folder_path != ""){
    caption= paste0("Bootstrapping sample statistics from sample with ",
                    ss," records")
    stat_summary |>
      knitr::kable(output = FALSE, caption = caption) |>
      kableExtra::kable_styling() |>
      kableExtra::as_image(file=paste0(folder_path,"Sampling_Statistics.png"))
  }


  if (summary$n !=  "auto"){
    bootstrap_summary =  summary |>
      dplyr::select(sample_size, rep,level,n) |>
      dplyr::mutate(level = round(1-level,3)) |>
      dplyr::rename("Sample Size"=sample_size,
             "Repetitons"=rep,
             "Significance Level"=level,
             "Samples per Bootstrap"= n)
  } else {
    bootstrap_summary =  summary |>
      dplyr::select(sample_size, rep,level) |>
      dplyr::mutate(level = round(1-level,3)) |>
      dplyr::rename("Sample Size"=sample_size,
             "Repetitons"=rep,
             "Significance Level"=level)
  }



  if (save == TRUE | folder_path != ""){
    caption= paste0("Bootstrapping Parameters ")
    bootstrap_summary |>
      knitr::kable(output = FALSE, caption = caption) |>
      kableExtra::kable_styling() |>
      kableExtra::as_image(file=paste0(folder_path,"Bootsrapping_table.png"))
  }

  return(list(stat_summary, bootstrap_summary))
}









