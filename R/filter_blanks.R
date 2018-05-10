
#' Filter aggregated data to the most granular level
#'
#'Create a filtered dataframe with no blanks
#'Only on the features!!
#'
#' @param data data.frame
#' @param features vector
#' @param all_symbol character
#'
#' @return data.frame
#' @export
#'
#' @examples
#' load("../example_data/yummy.Rda")
#' dat_2
#' filter_blanks(dat_2)
#'
filter_blanks <- function(data, features = NULL, all_symbol = ""){
  ### show data at the most granular level
  if (!is.data.frame(data)){
    stop("Error: data should be a dataframe!")
  }

  if (!is.null(features)){
    if(!is.vector(features)){
      stop("Error: features needs to be a vector of values")
    }
    features <- dplyr::intersect(names(data), features)
  }
  else {
    features <- names(data)
  }

  dat <- data %>%
    dplyr::filter_at(features, dplyr::all_vars(. != all_symbol)) #%>%
    #dplyr::mutate_if(is.factor, dplyr::na_if, y = all_symbol) %>%
    #stats::na.omit()
  return(dat)
}
