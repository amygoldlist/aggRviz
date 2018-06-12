################
# This function will draw a spider_plots of all features
# right now, it can only draw Ages & Time vs metrics
# for example, spider_plots(good_data, "Age", "2014-Q1")
# or spider_plots(good_data, "Age", "all") to draw all times automaticlly.
# to be continue.
################




#' spider plots
#'
#' This function will draw a spider_plots of all features
#' right now, it can only draw Ages & Time vs metrics
#' for example, spider_plots(good_data, "Age", "2014-Q1")
#' or spider_plots(good_data, "Age", "all") to draw all times automaticlly.
#' to be continue.
#'
#' @param good_data path
#' @param feature vector
#' @param Time character
#'
#' @return plot
#'
#'
#'
spider_plots <- function(good_data, feature, Time) {



  metrics <- identify_measures(names(good_data))
  features <- c()
  for (i in names(good_data)){
    if (i %in% metrics){}
    else{features = c(features,i)}
  }


  test <- good_data %>% aggRviz_filter2(col_2_keep = c(feature,"Time",metrics))


  if (feature == "Age"){

    if (time == "all"){
      for (i in unique(test$Time)){
        test5 <- test %>%
          filter(Time == i) %>%
          dplyr::select(-Time) %>%
          mutate_each(funs(rescale),-Age)

        print(ggradar(test5[,c(1:7)],
                      font.radar = "Times",
                      group.point.size = 1,
                      group.line.width = 1,
                      axis.label.size = 5,
                      legend.text.size = 10,
                      plot.title  = i))
        }


    }
    else{
      for (i in time){
        test5 <- test %>%
          dplyr::filter(Time == i) %>%
          dplyr::select(-Time) %>%
          mutate_each(funs(rescale),-Age)
      }
      print(ggradar(test5[,c(1:7)],
                    font.radar = "Times",
                    group.point.size = 1,
                    group.line.width = 1,
                    axis.label.size = 5,
                    legend.text.size = 10,
                    plot.title  = i))
    }

  }

}


