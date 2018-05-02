

#' Title
#'
#' @param data data.frame
#' @param key vector
#'
#' @return
#' @export
#'
#' @examples
identify_measures <- function(data, key){

  list_y <- c()

  #check key column in the second dataset
  for (i in 1:length(names(data))){
    if (T %in% stringr::str_detect(names(data)[i], key) == TRUE){
      list_y <-
        union(list_y, names(data)[i])

    }
  }
}
