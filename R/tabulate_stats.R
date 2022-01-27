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
#' @param precision A integer vector for the precision of the
#'                  table values
#' @param estimator A logical vector to include the bootstrap
#'                  estimate in the summary statistics table
#' @param alpha A logical vector to include the significance
#'              level in the summary statistics table
#'
#' @return table objects
#' @export
#'
#' @examples
#' st <- bootstrap_stats([1, 2, 3, 4], 1000, level=0.95,
#'                       random_seed=123)
#' stats_table, parameter_table  <-  summary_tables(st)
#' stats_table
#' parameter_table
tabulate_stats <- function(stat_list, precision=2, estimator=TRUE,
                           alpha=TRUE, save = FALSE) {

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
    save_kable(st_table, file = "Sampling_Statistics.png")
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
    save_kable(bs_table,file = "Bootstrap_parameter_table.png")
  }

  return(list(st_table, bs_table))
}









