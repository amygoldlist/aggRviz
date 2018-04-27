

#' Filter aggregated data to a chosen level
#'
#' The columns to delete are the features that you DO NOT want to stratify by.
#' This function filters out any row, stratified by those columns,
#' and filters out any unstratified rows from the other features.
#'
#'
#' @param dat data.frame
#' @param col_2_delete vector
#'
#'
#'
#' @return data.frame
#' @export
#'
#'
aggRviz_filter <- function(dat,col_2_delete){
 # if (!is.vector(col_2_delete)){
  #  stop("Error! col_2_delete needs to be a vector")
#  }

 # if (!is.data.frame(dat)){
  #  stop("Error! dat should be a dataframe")
#  }
 # if (union(names(dat), col_2_delete) != names(dat)){
  #  stop("col_2_delete contains soemthing that is not a column in the data.frame")
#  }

   keepers <- dplyr::setdiff(names(dat),col_2_delete)
  #print(keepers)
  dat <- dat %>%
    ## all_vars gets rid of all that have at least one, any gets rid of both
    dplyr::filter_at(col_2_delete, dplyr::all_vars(. =="")) %>%
    ### select only the good stuff
    dplyr::select(dplyr::one_of(keepers))
    ### kill all the blanks!!
  dat <- filter_blanks(dat)
  return(dat)
}



c(5,19) %in% c(5,6,7,8)







