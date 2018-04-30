
#' Filter aggregated data to the most granular level
#'
#'Create a filtered dataframe with no blanks
#'
#' @param data data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#'
filter_blanks <- function(data){
  ### show data at the most granular level
  if (!is.data.frame(data)){
    stop("Error: data should be a dataframe!")
  }
  dat <- data %>%
    dplyr::mutate_if(is.factor, dplyr::na_if, y='') %>%
    stats::na.omit()
  return(dat)
}
