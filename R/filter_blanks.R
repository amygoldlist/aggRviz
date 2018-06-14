
#' Filter aggregated data to the most granular level
#'
#'Create a filtered dataframe with no blanks
#' If you want to only look at certain column (ie, allow blanks in other columns), specify these as features.
#' If you want to filter out something besides blanks, use the variable all_symbol.
#' So setting all_symbol = "chocolate" filters out any row containing chocolate in the features columns
#'
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
  ### make sure data is a dataframe
  if (!is.data.frame(data)){
    stop("Error: data should be a dataframe!")
  }

  ## make sure features is NULL or a vector
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
    dplyr::filter_at(features, dplyr::all_vars(. != all_symbol))

  return(dat)
}
