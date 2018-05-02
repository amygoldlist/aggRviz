
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

aggRviz_join <- function(x,y, key = c("measure","rate"), col_2_ignore = NULL, all_symbol = ""){
  #x,y can be two original datasets

  if (!is.data.frame(x)){
    stop("Error: x should be a dataframe!")
  }
  if (!is.data.frame(y)){
    stop("Error: y should be a dataframe!")
  }

  #if (!is.null(col_2_ignore)){
    ### FILL IN THIS BIT!!
    ## rename each column in each dataframe
    ## by appending an x or y to the end of the column
  #}


  ### Get a list of columns in both
  common_columns <- intersect(names(x),names(y))


  ### find the key columns in x and y :
  list_x <- identify_measures(x, key = key)
  list_y <- identify_measures(y, key = key)



   # Get a list of columns in only 1 df

  delete_x <- names(x) %>%
    dplyr::setdiff(common_columns) %>%
    dplyr::setdiff(list_x) %>%
    dplyr::setdiff(col_2_ignore)

  delete_y <- names(y) %>%
    dplyr::setdiff(common_columns) %>%
    dplyr::setdiff(list_y) %>%
    dplyr::setdiff(col_2_ignore)



  ### filter to only blanks in the columns to delete
  ### deselect columns to delete

  x <- x %>%
    ## all_vars gets rid of all that have at least one, any gets rid of both
    dplyr::filter_at(delete_x, dplyr::all_vars(. == all_symbol)) %>%
    ### select only the good stuff
    dplyr::select(-dplyr::one_of(delete_x))

  y <- y %>%
    ## all_vars gets rid of all that have at least one, any gets rid of both
    dplyr::filter_at(delete_y, dplyr::all_vars(. == all_symbol)) %>%
    ### select only the good stuff
    dplyr::select(-dplyr::one_of(delete_y))


  ### inner_join the 2 datasets.
  dat <- dplyr::inner_join(x,y)


  ### NOW we would need to filter....


  return(dat)
}
