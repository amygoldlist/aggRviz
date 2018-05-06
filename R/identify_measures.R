

#' identify_measures
#'
#' Enter a data.frame or vector, and this function will return a vector of measures
#' Optional:  add a vector of names to recognize as measures,
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
#' load("example_data/yummy.Rda")
#' names(dat1)
#' identify_measures(dat_1)
#'
identify_measures <- function(data, key = c("measure", "rate")){

  if (!is.vector(data)){
    if(is.data.frame(data)){
      data <- names(data)
    } else{
      stop("Error! Data must be a dataframe or vector!")
    }
  }

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
