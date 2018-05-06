

#' Filter aggregated data to a chosen level
#' EXPERIMENT!!!
#'
#' The columns to delete are the features that you DO NOT want to stratify by.
#' This function filters out any row, stratified by those columns,
#' and filters out any unstratified rows from the other features.
#'
#' OPTION:  col_2_keep lets you filter "in"
#'
#' Option:  if it's not a blank for all categories, set all_symbol
#' This could be NA, or " " or "all".
#'
#' Option:  if you set features, it will only delete columns in that set
#'
#'
#' @param dat data.frame
#' @param col_2_delete vector
#' @param col_2_keep vector
#' @param all_symbol character
#'
#' @return data.frame
#' @export
#'
#' @example
#' load("example_data/yummy.Rda")
#' dat_1
#'
#' aggRviz_filter(data = dat_1,col_2_delete= c("dessert", "fruit"))
#'
#'
aggRviz_filter2 <- function(data,col_2_delete = NULL, col_2_keep = NULL, features = NULL, all_symbol = ""){
  if (!is.data.frame(data)){
    stop("Error: data should be a dataframe!")
  }

  ##check that exactly one of col_2_keep and col_2_delete is null
  if (is.null(col_2_delete) & is.null(col_2_keep)){
    stop("Error: enter exactly one of col_2_delete and col_2_keep!")
  }

  if (!is.null(col_2_delete) & !is.null(col_2_keep)){
    stop("Error: enter exactly one of col_2_delete and col_2_keep!")
  }

  ### checks for features:
  col_features <- names(data)

  if (!is.null(features)){
    if(!is.vector(features)){
      stop("Error: features needs to be a vector of values")
    }
    col_features <- dplyr::setdiff(names(data), features)
  }


  ### if we're looking at deleting columns.
  if (is.null(col_2_keep)){
    if (!is.vector(col_2_delete)){
      stop("Error! col_2_delete needs to be a vector!")
    }


    col_2_delete <- dplyr::intersect(col_2_delete, col_features)

    keepers <- dplyr::setdiff(col_features,col_2_delete)
  }

### if we're looking at keeping columns.
  if (is.null(col_2_delete)){
    if (!is.vector(col_2_keep)){
      stop("Error! col_2_keep needs to be a vector!")
    }


    keepers <- dplyr::intersect(col_2_keep, col_features)
    col_2_delete <- dplyr::setdiff(col_features, keepers)
  }


  dat <- data %>%
    ## all_vars gets rid of all that have at least one, any gets rid of both
    dplyr::filter_at(col_2_delete, dplyr::all_vars(. == all_symbol)) %>%
    ### select only the good stuff
    dplyr::select(dplyr::one_of(keepers))
    ### kill all the blanks!!
  dat <- filter_blanks(dat, all_symbol)

  return(dat)
}









