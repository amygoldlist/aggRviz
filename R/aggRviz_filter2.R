

#' Filter aggregated data to a chosen level
#' EXPERIMENT!!!
#'
#' The columns to delete are the features that you DO NOT want to stratify by.
#' This function filters out any row, stratified by those columns,
#' and filters out any unstratified rows from the other features.
#'
#' OPTION:  col_2_keep lets you filter "in"
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
aggRviz_filter2 <- function(data,col_2_delete = NULL, col_2_keep = NULL){
  if (!is.data.frame(data)){
    stop("Error: data should be a dataframe!")
  }

  ##check that exactly one of col_2_keep and col_2_delete is null
  if (is.null(col_2_delete) & is.null(col_2_keep)){
    stop("Error: enter exactly one of col_2_delete and col_2_keep!")
  }

  ##check that exactly one of col_2_keep and col_2_delete is null
  if (!is.null(col_2_delete) & !is.null(col_2_keep)){
    stop("Error: enter exactly one of col_2_delete and col_2_keep!")
  }

  ### if we're looking at deleting columns.
  if (is.null(col_2_keep)){
    if (!is.vector(col_2_delete)){
      stop("Error! col_2_delete needs to be a vector!")
    }

 # if (union(names(data), col_2_delete) != names(dat)){
  #  stop("col_2_delete contains soemthing that is not a column in the data.frame")

    keepers <- dplyr::setdiff(names(data),col_2_delete)
  }

### if we're looking at keeping columns.
  if (is.null(col_2_delete)){
    if (!is.vector(col_2_keep)){
      stop("Error! col_2_keep needs to be a vector!")
    }

# if (union(names(data), col_2_delete) != names(dat)){
#  stop("col_2_delete contains soemthing that is not a column in the data.frame")

  col_2_delete <- dplyr::setdiff(names(data),col_2_keep)
  keepers <- col_2_keep
  }


  dat <- data %>%
    ## all_vars gets rid of all that have at least one, any gets rid of both
    dplyr::filter_at(col_2_delete, dplyr::all_vars(. =="")) %>%
    ### select only the good stuff
    dplyr::select(dplyr::one_of(keepers))
    ### kill all the blanks!!
  dat <- filter_blanks(dat)
  return(dat)
}



c(5,19) %in% c(5,6,7,8)







