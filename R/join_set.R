

#' Join 2 tables on the maximum overlapping set of features
#'
#' This function will pick key column (such as "retention rate" or proportion rate)
#' and join two lowest datasets by common columns.
#'
#' @param x data.frame
#' @param y data.frame
#'
#' @return data.frame
#' @export
#'
#' @examples
#' join_set(A,B)
#' A is a dataset with feature "a","b","proportion rate"
#' B is a dataset with feature "a","c","interesting rate"
#' The output will be a dataset with feature "a", "proportion rate", "interesting rate"
#'
#'
join_set <- function(x,y){#x,y can be two original datasets
  list_x = c()
  list_y = c()
  #check common columns
  for (i in names(x)){
    if (i %in% names(y) == F){
      list_x = c(list_x,i)
    }
  }
  for (j in names(y)){
    if (j %in% names(x) == F){
      list_y = c(list_y,j)
    }
  }

  #check key column in the first dataset
  for (i in 1:length(list_x)){
    if (T %in% str_detect(list_x[i],c("measure","rate")) == T){
      list_x = list_x %>% setdiff(list_x[i])
    }
  }
  #check key column in the second dataset
  for (i in 1:length(list_y)){
    if (T %in% str_detect(list_y[i],c("measure","rate")) == T){
      list_y = list_y %>% setdiff(list_y[i])
    }
  }

  #use `filter_blanks()` and `aggrViz_filter()` to deal with blanks
  if (length(list_x) != 0){
    x = x %>% aggrViz_filter(list_x)}
  else {x = x %>% filter_blanks()}
  if (length(list_y) != 0){
    y = y %>% aggrViz_filter(list_y)}
  else {y = y %>% filter_blanks()}

  return(inner_join(x,y))
}
