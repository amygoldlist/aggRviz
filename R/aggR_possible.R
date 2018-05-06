
#' aggR_possible
#'
#' Checks through a dataframe and a vector of features that can be deleted
#' outputs a list of list
#' each sublist contains a set of features that can be filtered out.
#'
#' @param dat data.frame
#' @param feature_names vector
#'
#' @return filter_list list
#' @export
#'
#' @examples
#' load("../example_data/yummy.Rda")
#' dat_2
#'
#' aggR_possible(dat_2, c("Colour", "Sweet_or_Salty", "Fruit"))
#'
#'
aggR_possible <- function(dat, feature_names = names(dat)){

  ##create an empty list for holding data
  filter_list <- list()
  counter <- 1
  for (i in 1:length(feature_names)){
    feat_groups <- utils::combn(feature_names,i, simplify = FALSE)
    #print(length(feat_groups))
    for (j in 1:length(feat_groups)){
      feature <- feat_groups[[j]]
      #print(feature)
      df <- aggRviz_filter(dat, feature)
      if (nrow(df)>0) {
        filter_list[[counter]] <- feature
        counter <- counter+1
        #print(counter)
        #print("Subtract features:")
        #print(feature)
        #print(glue("has length: {nrow(df)}"))
      }
    }
  }

  return(filter_list)
}

