##################
# This function will generate k-prot cluster plots for all the combination of key
# features of all csv files in a certain path.
##################





generate_kprot_cluster <- function(path,n1=NULL){
  
  # read all .csv file 
  data_list <- read_all_csv_skip_n(path,n=2)
  
  vec <- list()
  len <- length(data_list)
  
  # create a list of all combinations
  for (i in 1:len){
    for (j in 1:len){
      if (j>i) {
        vec <- rlist::list.append(vec,c(i,j))
      }
    }
  }
  
  # print k-proto plots for key features
  for (x in vec){
    print(kprot_cluster(data_list[[x[1]]] %>% 
                          filter(Country == "US") %>% 
                          dplyr::select(-Confidence.interval.boundary,
                                        -Country,
                                        -Trend.quality) %>% 
                          aggRviz_filter2(col_2_delete = "State.or.Province"),
                        data_list[[x[2]]]%>% 
                          filter(Country == "US") %>%
                          dplyr::select(-Confidence.interval.boundary,
                                        -Country,
                                        -Trend.quality) %>% 
                          aggRviz_filter2(col_2_delete = "State.or.Province"),n1))
  }
}
