
#' read_all_csv_skip_n
#'
#' This function is used to read all csv in a folder
#' It return a list of all datasets.
#' examples: datalist <- read_all_csv_skip_n(path)
#' The first csv is datalist[[1]]
#' To generalize to other files, use pattern = "*.md", ect.
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
  ##errors:
  ## Errors for nonnumeric number
  if (!is.numeric(n)){
    stop("Error: number should be a nonnegative integer")
  }
  ## coerce into an integer, if it's a decimal
  number <- as.integer(n)

  if (n < 0){
    stop("Error: value of n needs to be nonnegative")
  }



  if (!is.character(path)){
    stop("Error: path needs to be a valid path!")
  } else if (!file.exists(path)){
       stop("Error: path needs to be a valid path!")
     }


  skipn <- function(x,y=n) utils::read.csv(x,skip = y)

  files = list.files(path, pattern, full.names = TRUE)
  lapply(files, skipn)
}
