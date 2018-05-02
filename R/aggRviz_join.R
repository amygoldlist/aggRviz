
## A is a dataset with feature "a","b","proportion rate"
## B is a dataset with feature "a","c","interesting rate"
## The output will be a dataset with feature "a", "proportion rate", "interesting rate"


#' Join 2 tables on the maximum overlapping set of features
#'
#' This function will pick key column (such as "interesting rate" or proportion rate)
#' and join two lowest datasets by common columns.
#'
#' THis is a new attempt, building on Fang's work
#'
#' The key is a vector of key words that make something important, (and included.)
#'
#' @param x data.frame
#' @param y data.frame
#' @param key vector
#' @param col_2_ignore vector
#' @param all_symbol
#'
#' @return data.frame
#' @export
#'@examples
#'
#'

aggRviz_join_set <- function(x,y, key = c("measure","rate"),col_2_ignore = NULL, all_symbol = ""){
  #x,y can be two original datasets

  if (!is.data.frame(x)){
    stop("Error: x should be a dataframe!")
  }
  if (!is.data.frame(y)){
    stop("Error: y should be a dataframe!")
  }

  if !is.null(col_2_ignore){
    ### FILL IN THIS BIT!!
    ## rename each column in each dataframe
    ## by appending an x or y to the end of the column
  }


  ### Get a list of columns in both
  common_columns <- intersect(names(x),names(y))

  ### Get a list of columns in only 1 df
  list_x <- dplyr::setdiff(names(x), common_columns)
  list_y <- dplyr::setdiff(names(y), common_columns)


  #check key column in the first dataset
  for (i in 1:length(list_x)){
    if (T %in% stringr::str_detect(list_x[i],key) == TRUE){
      list_x <-
        list_x %>%
        setdiff(list_x[i])
    }
  }

  #check key column in the second dataset
  for (i in 1:length(list_y)){
    if (T %in% stringr::str_detect(list_y[i], key) == TRUE){
      list_y <-
        list_y %>%
        setdiff(list_y[i])
    }
  }

  ####PASSS:
  ### BElow this isn't correct, we need to join first I think.



  #use `filter_blanks()` and `aggrViz_filter()` to deal with blanks
  if (length(list_x) != 0){
    x  <-  x %>%
      aggRviz_filter(list_x, all_symbol = all_symbol)
    }
  else {x = x %>% filter_blanks()}
  if (length(list_y) != 0){
    y = y %>% aggRviz_filter(list_y)}
  else {y = y %>% filter_blanks()}

  return(dplyr::inner_join(x,y))
}
