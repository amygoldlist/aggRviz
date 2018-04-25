

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
#' @importFrom magrittr %>%
#' @importFrom magrittr .
#'
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#'
aggRviz_filter <- function(dat,col_2_delete){
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









