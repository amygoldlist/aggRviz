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
    
    metrics <- identify_measures(names(test))
    
    features <- c()
    for (i in names(test)){
      if (i %in% metrics){}
      else{features = c(features,i)}
    }
    
    aaa <- features[! features %in% "Time"]
    
    
    
    test2 <- aggregate(test %>% dplyr::select(metrics), 
                       test %>% dplyr::select(aaa), 
                       mean) 
    
    kp <- kproto(test2,n)
    
    cluster_plot <- ggplot(test2,aes(x = test2[,length(test2)-1], 
                                     y = test2[,length(test2)]))+
      geom_point(aes(color = as.factor(kp$cluster)),palette="Set1",size=2)+
      geom_point(kp$centers,
                 mapping = aes(x = kp$centers[,length(kp$centers)-1], 
                               y =kp$centers[,length(kp$centers)],
                               color = as.factor(1:range(nrow(kp$centers)))),
                 palette="Set1",
                 size = 5,
                 shape = 21,stroke = 4)+
      xlab(metrics[1])+
      ylab(metrics[2])+
      labs(color="Cluster")
    
    cluster_center <- kp$centers
    
    #plot cluster map
    
    return(list(cluster_plot,cluster_center))
  }
  else{
    print(paste0("The row number of the lowest level dataset is less than ",n))
  }
}