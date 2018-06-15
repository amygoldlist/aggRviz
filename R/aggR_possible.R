
#' aggR_possible
#'
#' Checks through a dataframe and a vector of features that can be deleted
#' OR KEPT!!!!!
#' outputs a list of list
#' each sublist contains a set of features that can be filtered out.
#' To filter by keeping, set keep = TRUE (default)
#' To filter by columns to filter out, set keep = FALSE
#' if you want to ignore certain columns, use the features setting.
#' If you do, the function will ONLY consider these columns, ignoring the others.
#' If you only want to look at combinations of a certain length, use number.  Otherwise, leave it as null
#'
#' @param data data.frame
#' @param number integer
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
#' aggR_possible(dat_2, features =c("Colour", "Sweet_or_Salty", "Fruit"), number = 2, keep = TRUE)
#'
#'
aggR_possible <- function(data,
                          number = NULL,
                          features = names(data), keep = TRUE, all_symbol = ""){
  ### Errors
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

  if (length(feature_names) == 0){
    feature_names <- names(data)
  }


## If no number is selected:
  if(is.null(number)){
    for (i in 1:(length(feature_names)-1)){
      feat_groups <- utils::combn(feature_names,i, simplify = FALSE)
      for (j in 1:length(feat_groups)){
        feature <- feat_groups[[j]]

        if (!keep){
          df <- aggRviz_filter2(data, col_2_delete = feature, features = feature_names, all_symbol = "")
        }
        if (keep){
          df <- aggRviz_filter2(data, col_2_keep = feature, features = feature_names, all_symbol = "")
        }

        if (nrow(df)>0) {
          filter_list[[counter]] <- feature
          counter <- counter+1

        }
      }
    }
  } else {

    ## Errors for nonnumeric number
    if (!is.numeric(number)){
      stop("Error: number should be numeric or NULL")
    }
    ## coerce into an integer, if it's a decimal
    number <- as.integer(number)

    if (number <= 0 | number > length(features)){
      stop("Error: value of number is not possible")
    }


    feat_groups <- utils::combn(feature_names,number, simplify = FALSE)
    #print(length(feat_groups))
    for (j in 1:length(feat_groups)){
      feature <- feat_groups[[j]]
      if (!keep){
        df <- aggRviz_filter2(data, col_2_delete = feature, features = feature_names, all_symbol = "")
      }
      if (keep){
        df <- aggRviz_filter2(data, col_2_keep = feature, features = feature_names, all_symbol = "")
      }

      if (nrow(df)>0) {
        filter_list[[counter]] <- feature
        counter <- counter+1

      }

    }
  }

  return(filter_list)
}

