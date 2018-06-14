

#' identify_measures
#'
#' Enter a data.frame or vector, and this function will return a vector of measures
#' A measure is defined as a column name (or item in a vector) that contains a key word.
#' default is c("measure", "rate").
#' The function uses stringr to detect these regular expressions.
#'
#' @param data data.frame or vector
#' @param key vector
#'
#' @return vector
#' @export
#'
#' @examples
#' load("../example_data/yummy.Rda")
#' names(dat_1)
#' identify_measures(dat_1)
#'
#' names_to_check <- c("condition", "odds_ratio", "blood_type", "percent_ratio" )
#' identify_measures(names_to_check, key = "ratio")
#'
identify_measures <- function(data, key = c("measure", "rate")){

  ## check to see if data is a vector or dataframe
  if (!is.vector(data)){
    if(is.data.frame(data)){
      data <- names(data)
    } else{
      stop("Error! Data must be a dataframe or vector!")
    }
  }

  ## create an empty list
  list_y <- c()

  #check key column in the second dataset
  for (i in 1:length(data)){
    if (T %in% stringr::str_detect(data[i], key) == TRUE){
      list_y <-
        union(list_y, data[i])

    }
  }
  return(list_y)
}
