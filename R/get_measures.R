#' Title
#'
#' @param clean_dataset_path
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
get_measures <- function(clean_dataset_path){
  full_dataset <- read.csv(clean_dataset_path)

  good_data <- full_dataset %>%
    # filter(Trend.quality1=="Green",
    #        Trend.quality2=="Green",
    #        Trend.quality3=="Green",
    #        Trend.quality4=="Green",
    #        Trend.quality5=="Green",
    #        Trend.quality6=="Green",
    #        Trend.quality7=="Green",
    #        Trend.quality8=="Green",
    #        Trend.quality9=="Green",) %>%
    dplyr::select(Age,
                  Job.Function..EEOC.,
                  People.manager,
                  Performance.group,
                  Industry,
                  Tenure,
                  Country,
                  Region,
                  State.or.Province,
                  Time,
                  Gender,
                  gender_diversity_measure,
                  #headcount_Age_measure,
                  #headcount_Job.Function..EEOC._measure,
                  #headcount_People.manager_measure,
                  high_performers_ratio_measure,
                  managers_ratio_measure,
                  wfa_eeoc_minority_ratio_measure,
                  wfa_promotion_received_rate,
                  wfa_retention_turnover_resignation_n_rate) %>%
    stats::na.omit()
  return(good_data)
}
