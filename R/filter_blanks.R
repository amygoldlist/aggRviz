####################################################
## Make a filter to lowest level function
####################################################

filter_blanks <- function(data){
  ### show data at the most granular level
  dat <- data %>%
    mutate_if(is.factor, na_if, y='') %>%
    na.omit()
  return(dat)
}
