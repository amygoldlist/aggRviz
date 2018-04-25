### filter out any variables

##require(dplyr)

### example:  aggrViz(dat = my_df, col_2_delete = c("Gender", "Age"))


#' Title
#'
#' @param dat
#' @param col_2_delete
#'
#' @return
#' @export
#'
#' @examples
aggrViz_filter <- function(dat,col_2_delete){
  keepers <- dplyr::setdiff(names(dat),col_2_delete)
  #print(keepers)
  dat <- dat %>%
    ## all_vars gets rid of all that have at least one, any gets rid of both
    dplyr::filter_at(col_2_delete, all_vars(. =="")) %>%
    ### select only the good stuff
    dplyr::select(one_of(keepers))
    ### kill all the blanks!!
  dat <- filter_blanks(dat)
  return(dat)
}









