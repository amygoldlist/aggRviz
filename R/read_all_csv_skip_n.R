###########
# This function is used to read all csv in a folder
# It return a list of all datasets.
# examples: datalist <- read_all_csv_skip_n(path)
# The first csv is datalist[[1]]
###########


#' read_all_csv_skip_n
#'
#' This function is used to read all csv in a folder
#' It return a list of all datasets.
#' examples: datalist <- read_all_csv_skip_n(path)
#' The first csv is datalist[[1]]
#'
#' @param path character
#' @param n integer
#' @param pattern character
#'
#' @return list
#' @export
#'
#'
#'
read_all_csv_skip_n<- function(path,n=2, pattern = "*.csv") {

  skipn <- function(x,y=n) utils::read.csv(x,skip = y)

  files = list.files(path, pattern, full.names = TRUE)
  lapply(files, skipn)
}
