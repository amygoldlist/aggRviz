
#' aggR_possible
#'
#' Checks through a dataframe and a vector of features that can be deleted
#' OR KEPT!!!!!
#' outputs a list of list
#' each sublist contains a set of features that can be filtered out.
#' To filter by keeping, set keep = TRUE (default)
#' To filter by columns to filter out, set keep = FALSE
#'
#' @param data data.frame
#' @param features vector
#' @param keep logical
#' @param all_symbol character
#'
#' @return filter_list list
#' @export
#'
#' @examples
#' load("../example_data/yummy.Rda")
#' dat_2
#'
#' aggR_possible(dat_2, features =c("Colour", "Sweet_or_Salty", "Fruit"), keep = TRUE)
#'
#'
aggR_possible <- function(data, features = names(data), keep = TRUE, all_symbol = ""){
  ### ADD ERRORS!!!
  if (!is.data.frame(data)){
    stop("Error: data should be a dataframe!")
  }

  if(!is.vector(features)){
    stop("Error: features needs to be a vector of values")
  }

  if(!is.logical(keep)){
    stop("Error: keep should be TRUE or FALSE")
  }

  ##create an empty list for holding data
  filter_list <- list()
  counter <- 1
  ##
  feature_names <-  dplyr::intersect(features, names(data))

  for (i in 1:length(feature_names)-1){
    feat_groups <- utils::combn(feature_names,i, simplify = FALSE)
    #print(length(feat_groups))
    for (j in 1:length(feat_groups)){
      feature <- feat_groups[[j]]
      #print(feature)
      if (!keep){
        df <- aggRviz_filter2(data, col_2_delete = feature, features = feature_names, all_symbol = "")
      }
      if (keep){
        df <- aggRviz_filter2(data, col_2_keep = feature, features = feature_names, all_symbol = "")
      }

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

  if (keep){
    filter_list[[counter]] <- features
  }

  return(filter_list)
}

