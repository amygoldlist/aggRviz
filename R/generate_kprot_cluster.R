##################
# This function will generate k-prot cluster plots for all the combination of key
# features of all csv files in a certain path.
##################





generate_kprot_cluster <- function(path,n, Confidence.interval.boundary = c()){

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
                    dplyr::select(-Confidence.interval.boundary),
                  data_list[[x[2]]]%>%
                    dplyr::select(-Confidence.interval.boundary), n))
  }
}
