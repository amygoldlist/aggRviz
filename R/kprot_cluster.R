################
# This function do k-prototypes cluster for 2 lowest datasets.
# Since each metric only contain 1 key feature.
# This function will identify two key features for the two dataset you input,
# and plot clusering map with two key features as x,y axis.
################



# dat1, dat2 should be the clean dataset with no blank.
# n is the cluster number
#' Title
#'
#' @param dat1 data.frame
#' @param dat2 data.frame
#' @param n integer
#'
#' @return plot
#' @export
#'
#'
#'
kprot_cluster <- function(dat1,dat2,n){
  #join two dataset
  test <- join_set(dat1,dat2)
  
  if (nrow(test) >= n){
    
    #k-prototype cluster
    kp <- clustMixType::kproto(test,n)
    
    #identify key features
    key <- c()
    for (i in 1:length(names(test))){
      if (T %in% stringr::str_detect(names(test)[i],c("measure","rate")) == T){
        key = c(key,i)
      }
    }
    
    #plot cluster map
    
    return(graphics::plot(test[,key], col = kp$cluster))
  }
  else{
    print(paste0("The row number of the lowest level dataset is less than ",n))
  }
}