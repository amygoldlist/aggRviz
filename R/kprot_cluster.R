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
kprot_cluster <- function(dat1,dat2,n = NULL){
  #load("../Rdata/filtering_4_keeps.Rda") firsly
  
  test <- join_set(dat1,dat2)
  
  metrics <- identify_measures(names(test))
  feat <- c()
  for (i in names(test)){
    if (i %in% metrics){}
    else{feat = c(feat,i)}
  }
  print(metrics)
  dat <- master_data %>% 
    filter(Country=="US",State.or.Province=="") %>% 
    dplyr::select(feat,metrics)
  
  
  
  
  # if (T %in% is.na(s2)){
  #   selected <- s2[!is.na(s2)]
  # }else{
  #   selected <- s2
  # }
  
  #poss <- aggR_possible(dat,keep = TRUE)
  #selected <- tail(poss,1)[[1]]
  
  if (is.null(n)){
    combination <- as.data.frame(pair_list) %>% mutate(id = row_number())
    filter_result <- combination %>% filter(V1==metrics[1],V2==metrics[2])
    selected_id <- filter_result$id
    
    s1 <- two_possibilities[[selected_id]][[length(two_possibilities[[selected_id]])]]
    s2 <- s1[s1!="State.or.Province"][s1!="Country"][s1!="Trend.quality"][s1!="Confidence.interval.boundary"]
    selected <- s2[s2 %in% feat]
    
    
    test1 <- dat %>% dplyr::select(selected,metrics) %>% filter_blanks() %>% na.omit()
    #dat %>% aggRviz_filter2(col_2_keep = c(selected,metrics,"Time")) %>% na.omit()
    
    feat1 <- c()
    for (i in names(test1)){
      if (i %in% metrics){}
      else{feat1 = c(feat1,i)}
    }
    
    aaa <- feat1[! feat1 %in% "Time"]
    
    test2 <- aggregate(test1 %>% dplyr::select(metrics), 
                       test1 %>% dplyr::select(aaa), 
                       mean)
    
    k.max <- 5
    
    k <- pamk(test2[,(length(test2)-1):length(test2)],krange = 2:k.max)$nc
    
    
    
    # wss <- sapply(1:k.max, 
    #               function(k){kproto(test2, k)$tot.withinss})
    # wss
    # plot(1:k.max, wss,
    #      type="b", pch = 19, frame = FALSE, 
    #      xlab="Number of clusters K",
    #      ylab="Total within-clusters sum of squares")
    # library(fpc)
    #   
    # 
    # test2 %>% head
    
    kp <- kproto(test2,k)
    
    
    label_list <- c()
    for(i in 1:nrow(kp$centers)) {
      label_list <- c(label_list,paste(unlist(kp$centers[i,1:(length(kp$centers)-2)]),
                                       collapse = ","))
    }
    
    point_list <- c()
    for(i in 1:nrow(test2)){
      point_list <- c(point_list,paste(unlist(test2[i,1:(length(test2)-2)]),
                                       collapse = ","))
    }
    
    
    display_name1 <- c()
    
    if("Age" %in% aaa){
      
      label_age <- c()
      bbb <- kp$centers %>% dplyr::select("Age")
      for(i in 1:nrow(kp$centers)) {
        label_age <- c(label_age,paste(unlist(bbb[i,1]),
                                       collapse = ","))
      }
      
      point_age <- c()
      ccc <- test2 %>% dplyr::select("Age")
      for(i in 1:nrow(test2)){
        point_age <- c(point_age,paste(unlist(ccc[i,1]),
                                       collapse = ","))
      }
      for (z in 1:length(unique(kp$cluster))){
        
        age_range <- unique(point_age[kp$cluster==z]) %>% 
          str_replace_all(" yrs","") %>% sort()
        
        
        new_age <- list()
        new_age[[1]] <- c(age_range[1] %>% substr(1,2))
        count <- 1
        
        if (age_range[length(age_range)] != "65+"){
          if(length(age_range)==1){
            new_age[[1]] <- age_range[1]
          }else{
            for (q in 1:(length(age_range)-1)){
              
              e <- nchar(age_range[q])
              f <- nchar(age_range[q+1])
              if (as.integer(age_range[q] %>% substr(e-1,e)) + 1 == as.integer(age_range[q+1] %>% substr(1,2))) {
                new_age[[count]][2] <- age_range[q+1] %>% substr(f-1,f) 
              }
              else{
                new_age[[count]][2] <- age_range[q] %>% substr(e-1,e)
                new_age[[count+1]] <- age_range[q+1] %>% substr(f-1,f)
                count <- count + 1
                new_age[[count]][2] <- age_range[q+1] %>% substr(f-1,f)
              }
            }
          }
        }else{
          if(length(age_range)==1){
            new_age[[1]] <- "65+"
          }else if (length(age_range)==2){
            e <- nchar(age_range[1])
            if (age_range[1] %>% substr(e-1,e)=="64"){
              new_age[[1]] <- "60-65+"
            }else{
              new_age[[1]] <- age_range[1]
              new_age[[2]] <- "65+"
            }
          }else{
            for (i in 1:(length(age_range)-2)){
              e <- nchar(age_range[i])
              f <- nchar(age_range[i+1])
              if (as.integer(age_range[i] %>% substr(e-1,e)) + 1 == as.integer(age_range[i+1] %>% substr(1,2))) {
                new_age[[count]][2] <- age_range[i+1] %>% substr(f-1,f) 
              }
              else{
                new_age[[count]][2] <- age_range[i] %>% substr(e-1,e)
                new_age[[count+1]] <- age_range[i+1] %>% substr(1,2)
                count <- count + 1
                new_age[[count]][2] <- age_range[i+1] %>% substr(f-1,f)
              }
            }
            if(new_age[[count]][2] == "64") new_age[[count]][2] <- "65+"
            else new_age[[count+1]] <- "65+"
          }
        }
        
        
        
        display_name <- c()
        for(i in 1:length(new_age)) display_name <- c(display_name,paste(new_age[[i]],collapse = "-"))
        display_name1 <- c(display_name1,paste(display_name,collapse=","))
      }
      display_age_range <- c()
      for (i in 1:length(label_list)){
        display_age_range <- c(display_age_range,paste0(label_list[i], "\n",display_name1[i]," yrs"))
      }
      
    }else if("Tenure" %in% aaa){
      label_age <- c()
      bbb <- kp$centers %>% dplyr::select("Tenure")
      for(i in 1:nrow(kp$centers)) {
        label_age <- c(label_age,paste(unlist(bbb[i,1]),
                                       collapse = ","))
      }
      
      point_age <- c()
      ccc <- test2 %>% dplyr::select("Tenure")
      for(i in 1:nrow(test2)){
        point_age <- c(point_age,paste(unlist(ccc[i,1]),
                                       collapse = ","))
      }
      
      for (z in 1:length(unique(kp$cluster))){
        
        tenure1 <- unique(point_age[kp$cluster==z]) %>% 
          str_replace_all(" yrs","") %>% 
          str_replace_all(" yr","") %>% sort()
        tenure_range <- c()
        for (i in tenure){
          if (i %in% tenure1) tenure_range <- c(tenure_range,i) 
        }
        
        new_tenure <- list()
        ################
        ################
        ################ should be substr(1,2)
        new_tenure[[1]] <- c(tenure_range[1] %>% substr(1,2))
        #tenure_range <- tenure_range[-1]
        count <- 1
        
        if(length(tenure_range)==1){
          # if(as.integer(new_tenure[[1]]) == tenure_range %>% substr(1,1) |
          #    as.integer(new_tenure[[1]])+1 == tenure_range %>% substr(1,1)){
          #   new_tenure[[1]][2] <- tenure_range %>% substr(nchar(tenure_range),nchar(tenure_range))
          # }else{
          #      new_tenure[[2]] <-  tenure_range
          #    }
          new_tenure[[1]] <- tenure_range[1]
        }else if (tenure_range[length(tenure_range)] == "15+"){
          if (length(tenure_range)==1) {
            new_tenure[[1]] <- "15+"
          }else{
            if(tenure_range[length(tenure_range)-1] == "10-14"){
              if (length(tenure_range)==2){
                new_tenure[[1]] <- "10"
                new_tenure[[1]][2] <- "15+"
              }else if(length(tenure_range)==3){
                e <- nchar(tenure_range[1])
                if (tenure_range[1] %>% substr(e,e) == "9"){
                  new_tenure[[1]] <- "5"
                  new_tenure[[1]][2] <- "15+"
                }else{
                  new_tenure <- tenure_range[1]
                  new_tenure[[2]] <- "10-15+"
                }
              }else{ 
                for (q in 1:(length(tenure_range)-3)){
                  
                  e <- nchar(tenure_range[q])
                  f <- nchar(tenure_range[q+1])
                  if (as.integer(tenure_range[q] %>% substr(e,e)) + 1 == as.integer(tenure_range[q+1] %>% substr(1,1))|
                      as.integer(tenure_range[q] %>% substr(e,e)) == as.integer(tenure_range[q+1] %>% substr(1,1))) {
                    new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f) 
                  }
                  else{
                    new_tenure[[count]][2] <- tenure_range[q] %>% substr(e,e)
                    new_tenure[[count+1]] <- tenure_range[q+1] %>% substr(1,1)
                    count <- count + 1
                    new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f)
                  }
                }
                
                if(new_tenure[[count]][2] == "9") new_tenure[[count]][2] <- "15+"
                else new_tenure[[count+1]] <- "15+"
              }
            }
            
            
            else{
              if(length(tenure_range)==2){
                new_tenure[[1]] <- tenure_range[1]
                new_tenure[[2]] <- "15+"
              }else if (length(tenure_range)>2){
                for (q in 1:(length(tenure_range)-2)){
                  
                  e <- nchar(tenure_range[q])
                  f <- nchar(tenure_range[q+1])
                  if (as.integer(tenure_range[q] %>% substr(e,e)) + 1 == as.integer(tenure_range[q+1] %>% substr(1,1))|
                      as.integer(tenure_range[q] %>% substr(e,e))  == as.integer(tenure_range[q+1] %>% substr(1,1))) {
                    new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f) 
                  }
                  else{
                    new_tenure[[count]][2] <- tenure_range[q] %>% substr(e,e)
                    new_tenure[[count+1]] <- tenure_range[q+1] %>% substr(1,1)
                    count <- count + 1
                    new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f)
                  }
                }
                new_tenure[[count+1]] <- "15+"
              }
              
            }
          }  
          
        }else if (tenure_range[length(tenure_range)] == "10-14"){
          if (length(tenure_range)==1){
            new_tenure[[1]] <- "10-14"
          }else if (length(tenure_range)==2){
            e <- nchar(tenure_range[1])
            if(tenure_range[1] %>% substr(e,e) == "9"){
              new_tenure[[1]] <- "5-14"
            }else{
              new_tenure[[1]] <- tenure_range[1]
              new_tenure[[2]] <- "10-14"
            } 
          }else{
            for (q in 1:(length(tenure_range)-2)){
              
              e <- nchar(tenure_range[q])
              f <- nchar(tenure_range[q+1])
              if (as.integer(tenure_range[q] %>% substr(e,e)) + 1 == as.integer(tenure_range[q+1] %>% substr(1,1))|
                  as.integer(tenure_range[q] %>% substr(e,e))  == as.integer(tenure_range[q+1] %>% substr(1,1))) {
                new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f) 
              }
              else{
                new_tenure[[count]][2] <- tenure_range[q] %>% substr(e,e)
                new_tenure[[count+1]] <- tenure_range[q+1] %>% substr(1,1)
                count <- count + 1
                new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f)
              }
            }
            if(new_tenure[[count]][2] == "9") new_tenure[[count]][2] <- "14"
            else new_tenure[[count+1]] <- "10-14"
          }  
        }else{
          for (q in 1:(length(tenure_range)-1)){
            
            e <- nchar(tenure_range[q])
            f <- nchar(tenure_range[q+1])
            if (as.integer(tenure_range[q] %>% substr(e,e)) + 1 == as.integer(tenure_range[q+1] %>% substr(1,1))|
                as.integer(tenure_range[q] %>% substr(e,e))  == as.integer(tenure_range[q+1] %>% substr(1,1))) {
              new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f) 
            }
            else{
              new_tenure[[count]][2] <- tenure_range[q] %>% substr(e,e)
              new_tenure[[count+1]] <- tenure_range[q+1] %>% substr(1,1)
              count <- count + 1
              new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f)
            }
          }
        }
        display_name <- c()
        for(i in 1:length(new_tenure)) display_name <- c(display_name,paste(new_tenure[[i]],collapse = "-"))
        display_name1 <- c(display_name1,paste(display_name,collapse=",")) 
      }
      
      ###############
      ###############
      
      display_age_range <- c()
      for (i in 1:length(label_list)){
        display_age_range <- c(display_age_range,paste0(label_list[i], "\n",display_name1[i]," yrs"))
      }  
    }else{
      display_age_range <- label_list
    }
    
    
    
    
    cluster_plot <- ggplot(test2,aes(x = test2[,length(test2)-1], 
                                     y = test2[,length(test2)]))+
      geom_point(aes(color = as.factor(kp$cluster)),size=2)+
      #geom_text(label = unique(point_list),check_overlap = TRUE,size = 5)+
      geom_smooth(aes(color = as.factor(kp$cluster)),method = "lm",se=F)+
      geom_encircle(aes(fill=as.factor(kp$cluster)),color = NA,alpha=0.2)+
      geom_point(kp$centers,
                 mapping = aes(x = kp$centers[,length(kp$centers)-1], 
                               y =kp$centers[,length(kp$centers)],
                               color = as.factor(1:range(nrow(kp$centers)))),
                 size = 5,
                 shape = 21,stroke = 4)+
      guides(fill=FALSE)+
      xlab(metrics[1])+
      ylab(metrics[2])+
      geom_label_repel(kp$centers,
                       mapping = aes(x = kp$centers[,length(kp$centers)-1], 
                                     y =kp$centers[,length(kp$centers)]),
                       label = display_age_range)+
      theme(legend.position="none")+
      ggtitle(paste0("Dimensions: ",paste(names(test2)[1:(length(test2)-2)],collapse = ",")))
    
    cluster_center <- kp$centers
    
    return(list(cluster_plot,cluster_center,kp$lambda))
  }
  else{
    
    s1 <- names(dat)
    s2 <- s1[s1!="State.or.Province"][s1!="Country"][s1!="Trend.quality"][s1!="Confidence.interval.boundary"][s1!="Time"]
    poss <- aggR_possible(dat,features = s2, number = n+2 ,keep = TRUE)
    
    for(j in 1:length(poss)) {
      
      test1 <- dat %>% 
        aggRviz_filter2(col_2_keep = c(poss[[j]],metrics,"Time")) %>% 
        na.omit()
      
      
      feat1 <- c()
      for (i in names(test1)){
        if (i %in% metrics){}
        else{feat1 = c(feat1,i)}
      }
      
      aaa <- feat1[! feat1 %in% "Time"]
      print(c(j,aaa,metrics)) 
      test2 <- test1 %>% dplyr::select(aaa,metrics)
      # test2 <- aggregate(test1 %>% dplyr::select(metrics), 
      #                test1 %>% dplyr::select(aaa), 
      #                mean)
      
      if (nrow(test2)<=5) next
      
      k <- pamk(test2[,(length(test2)-1):length(test2)],krange = 1:5)$nc
      
      if (k==1) next
      # wss <- sapply(1:k.max, 
      #               function(k){kproto(test2, k)$tot.withinss})
      # wss
      # plot(1:k.max, wss,
      #      type="b", pch = 19, frame = FALSE, 
      #      xlab="Number of clusters K",
      #      ylab="Total within-clusters sum of squares")
      # library(fpc)
      #   
      # 
      # test2 %>% head
      
      kp <- kproto(test2,k)
      
      
      label_list <- c()
      for(i in 1:nrow(kp$centers)) {
        label_list <- c(label_list,paste(unlist(kp$centers[i,1:(length(kp$centers)-2)]),
                                         collapse = ","))
      }
      
      
      point_list <- c()
      for(i in 1:nrow(test2)){
        point_list <- c(point_list,paste(unlist(test2[i,1:(length(kp$centers)-2)]),
                                         collapse = ","))
      }
      
      
      
      
      tenure <- c("<1","1","2","3-4","5-9","10-14","15+")
      
      
      
      # age_range <- c("20-24","25-29","30-34","40-64","65+")
      #   age_range
      
      display_name1 <- c()
      
      if("Age" %in% aaa){
        
        label_age <- c()
        bbb <- kp$centers %>% dplyr::select("Age")
        for(i in 1:nrow(kp$centers)) {
          label_age <- c(label_age,paste(unlist(bbb[i,1]),
                                         collapse = ","))
        }
        
        point_age <- c()
        ccc <- test2 %>% dplyr::select("Age")
        for(i in 1:nrow(test2)){
          point_age <- c(point_age,paste(unlist(ccc[i,1]),
                                         collapse = ","))
        }
        for (z in 1:length(unique(kp$cluster))){
          
          age_range <- unique(point_age[kp$cluster==z]) %>% 
            str_replace_all(" yrs","") %>% sort()
          
          
          new_age <- list()
          new_age[[1]] <- c(age_range[1] %>% substr(1,2))
          count <- 1
          
          if (age_range[length(age_range)] != "65+"){
            if(length(age_range)==1){
              new_age[[1]] <- age_range[1]
            }else{
              for (q in 1:(length(age_range)-1)){
                
                e <- nchar(age_range[q])
                f <- nchar(age_range[q+1])
                if (as.integer(age_range[q] %>% substr(e-1,e)) + 1 == as.integer(age_range[q+1] %>% substr(1,2))) {
                  new_age[[count]][2] <- age_range[q+1] %>% substr(f-1,f) 
                }
                else{
                  new_age[[count]][2] <- age_range[q] %>% substr(e-1,e)
                  new_age[[count+1]] <- age_range[q+1] %>% substr(f-1,f)
                  count <- count + 1
                  new_age[[count]][2] <- age_range[q+1] %>% substr(f-1,f)
                }
              }
            }
          }else{
            if(length(age_range)==1){
              new_age[[1]] <- "65+"
            }else if (length(age_range)==2){
              e <- nchar(age_range[1])
              if (age_range[1] %>% substr(e-1,e)=="64"){
                new_age[[1]] <- "60-65+"
              }else{
                new_age[[1]] <- age_range[1]
                new_age[[2]] <- "65+"
              }
            }else{
              for (i in 1:(length(age_range)-2)){
                e <- nchar(age_range[i])
                f <- nchar(age_range[i+1])
                if (as.integer(age_range[i] %>% substr(e-1,e)) + 1 == as.integer(age_range[i+1] %>% substr(1,2))) {
                  new_age[[count]][2] <- age_range[i+1] %>% substr(f-1,f) 
                }
                else{
                  new_age[[count]][2] <- age_range[i] %>% substr(e-1,e)
                  new_age[[count+1]] <- age_range[i+1] %>% substr(1,2)
                  count <- count + 1
                  new_age[[count]][2] <- age_range[i+1] %>% substr(f-1,f)
                }
              }
              if(new_age[[count]][2] == "64") new_age[[count]][2] <- "65+"
              else new_age[[count+1]] <- "65+"
            }
          }
          
          
          
          display_name <- c()
          for(i in 1:length(new_age)) display_name <- c(display_name,paste(new_age[[i]],collapse = "-"))
          display_name1 <- c(display_name1,paste(display_name,collapse=","))
        }
        display_age_range <- c()
        for (i in 1:length(label_list)){
          display_age_range <- c(display_age_range,paste0(label_list[i], "\n",display_name1[i]," yrs"))
        }
        
      }else if("Tenure" %in% aaa){
        label_age <- c()
        bbb <- kp$centers %>% dplyr::select("Tenure")
        for(i in 1:nrow(kp$centers)) {
          label_age <- c(label_age,paste(unlist(bbb[i,1]),
                                         collapse = ","))
        }
        
        point_age <- c()
        ccc <- test2 %>% dplyr::select("Tenure")
        for(i in 1:nrow(test2)){
          point_age <- c(point_age,paste(unlist(ccc[i,1]),
                                         collapse = ","))
        }
        
        for (z in 1:length(unique(kp$cluster))){
          
          tenure1 <- unique(point_age[kp$cluster==z]) %>% 
            str_replace_all(" yrs","") %>% 
            str_replace_all(" yr","") %>% sort()
          tenure_range <- c()
          for (i in tenure){
            if (i %in% tenure1) tenure_range <- c(tenure_range,i) 
          }
          
          new_tenure <- list()
          ################
          ################
          ################ should be substr(1,2)
          new_tenure[[1]] <- c(tenure_range[1] %>% substr(1,2))
          #tenure_range <- tenure_range[-1]
          count <- 1
          
          if(length(tenure_range)==1){
            # if(as.integer(new_tenure[[1]]) == tenure_range %>% substr(1,1) |
            #    as.integer(new_tenure[[1]])+1 == tenure_range %>% substr(1,1)){
            #   new_tenure[[1]][2] <- tenure_range %>% substr(nchar(tenure_range),nchar(tenure_range))
            # }else{
            #      new_tenure[[2]] <-  tenure_range
            #    }
            new_tenure[[1]] <- tenure_range[1]
          }else if (tenure_range[length(tenure_range)] == "15+"){
            if (length(tenure_range)==1) {
              new_tenure[[1]] <- "15+"
            }else{
              if(tenure_range[length(tenure_range)-1] == "10-14"){
                if (length(tenure_range)==2){
                  new_tenure[[1]] <- "10"
                  new_tenure[[1]][2] <- "15+"
                }else if(length(tenure_range)==3){
                  e <- nchar(tenure_range[1])
                  if (tenure_range[1] %>% substr(e,e) == "9"){
                    new_tenure[[1]] <- "5"
                    new_tenure[[1]][2] <- "15+"
                  }else{
                    new_tenure <- tenure_range[1]
                    new_tenure[[2]] <- "10-15+"
                  }
                }else{ 
                  for (q in 1:(length(tenure_range)-3)){
                    
                    e <- nchar(tenure_range[q])
                    f <- nchar(tenure_range[q+1])
                    if (as.integer(tenure_range[q] %>% substr(e,e)) + 1 == as.integer(tenure_range[q+1] %>% substr(1,1))|
                        as.integer(tenure_range[q] %>% substr(e,e)) == as.integer(tenure_range[q+1] %>% substr(1,1))) {
                      new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f) 
                    }
                    else{
                      new_tenure[[count]][2] <- tenure_range[q] %>% substr(e,e)
                      new_tenure[[count+1]] <- tenure_range[q+1] %>% substr(1,1)
                      count <- count + 1
                      new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f)
                    }
                  }
                  
                  if(new_tenure[[count]][2] == "9") new_tenure[[count]][2] <- "15+"
                  else new_tenure[[count+1]] <- "15+"
                }
              }
              
              
              else{
                if(length(tenure_range)==2){
                  new_tenure[[1]] <- tenure_range[1]
                  new_tenure[[2]] <- "15+"
                }else if (length(tenure_range)>2){
                  for (q in 1:(length(tenure_range)-2)){
                    
                    e <- nchar(tenure_range[q])
                    f <- nchar(tenure_range[q+1])
                    if (as.integer(tenure_range[q] %>% substr(e,e)) + 1 == as.integer(tenure_range[q+1] %>% substr(1,1))|
                        as.integer(tenure_range[q] %>% substr(e,e))  == as.integer(tenure_range[q+1] %>% substr(1,1))) {
                      new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f) 
                    }
                    else{
                      new_tenure[[count]][2] <- tenure_range[q] %>% substr(e,e)
                      new_tenure[[count+1]] <- tenure_range[q+1] %>% substr(1,1)
                      count <- count + 1
                      new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f)
                    }
                  }
                  new_tenure[[count+1]] <- "15+"
                }
                
              }
            }  
            
          }else if (tenure_range[length(tenure_range)] == "10-14"){
            if (length(tenure_range)==1){
              new_tenure[[1]] <- "10-14"
            }else if (length(tenure_range)==2){
              e <- nchar(tenure_range[1])
              if(tenure_range[1] %>% substr(e,e) == "9"){
                new_tenure[[1]] <- "5-14"
              }else{
                new_tenure[[1]] <- tenure_range[1]
                new_tenure[[2]] <- "10-14"
              } 
            }else{
              for (q in 1:(length(tenure_range)-2)){
                
                e <- nchar(tenure_range[q])
                f <- nchar(tenure_range[q+1])
                if (as.integer(tenure_range[q] %>% substr(e,e)) + 1 == as.integer(tenure_range[q+1] %>% substr(1,1))|
                    as.integer(tenure_range[q] %>% substr(e,e))  == as.integer(tenure_range[q+1] %>% substr(1,1))) {
                  new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f) 
                }
                else{
                  new_tenure[[count]][2] <- tenure_range[q] %>% substr(e,e)
                  new_tenure[[count+1]] <- tenure_range[q+1] %>% substr(1,1)
                  count <- count + 1
                  new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f)
                }
              }
              if(new_tenure[[count]][2] == "9") new_tenure[[count]][2] <- "14"
              else new_tenure[[count+1]] <- "10-14"
            }  
          }else{
            for (q in 1:(length(tenure_range)-1)){
              
              e <- nchar(tenure_range[q])
              f <- nchar(tenure_range[q+1])
              if (as.integer(tenure_range[q] %>% substr(e,e)) + 1 == as.integer(tenure_range[q+1] %>% substr(1,1))|
                  as.integer(tenure_range[q] %>% substr(e,e))  == as.integer(tenure_range[q+1] %>% substr(1,1))) {
                new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f) 
              }
              else{
                new_tenure[[count]][2] <- tenure_range[q] %>% substr(e,e)
                new_tenure[[count+1]] <- tenure_range[q+1] %>% substr(1,1)
                count <- count + 1
                new_tenure[[count]][2] <- tenure_range[q+1] %>% substr(f,f)
              }
            }
          }
          display_name <- c()
          for(i in 1:length(new_tenure)) display_name <- c(display_name,paste(new_tenure[[i]],collapse = "-"))
          display_name1 <- c(display_name1,paste(display_name,collapse=",")) 
        }
        
        ###############
        ###############
        
        display_age_range <- c()
        for (i in 1:length(label_list)){
          display_age_range <- c(display_age_range,paste0(label_list[i], "\n",display_name1[i]," yrs"))
        }  
      }else{
        display_age_range <- label_list
      }
      
      
      
      label_display <- c()
      for(i in 1:length(unique(kp$cluster))){
        label_display <- c(label_display, point_list[kp$cluster==i] %>%
                             str_replace(label_list[i],""))
      }
      
      
      
      
      
      
      
      cluster_plot <- ggplot(test2,aes(x = test2[,length(test2)-1], 
                                       y = test2[,length(test2)]))+
        geom_point(aes(color = as.factor(kp$cluster)),size=2)+
        geom_smooth(aes(color = as.factor(kp$cluster)),method = "lm",se=F)+
        geom_encircle(aes(fill=as.factor(kp$cluster)),color = NA,alpha=0.2)+
        geom_point(kp$centers,
                   mapping = aes(x = kp$centers[,length(kp$centers)-1], 
                                 y =kp$centers[,length(kp$centers)],
                                 color = as.factor(1:range(nrow(kp$centers)))),
                   size = 5,
                   shape = 21,stroke = 4)+
        geom_text(label = label_display,alpha=0.7,check_overlap = T)+
        guides(fill=FALSE)+
        xlab(metrics[1])+
        ylab(metrics[2])+
        geom_label_repel(kp$centers,
                         mapping = aes(x = kp$centers[,length(kp$centers)-1], 
                                       y =kp$centers[,length(kp$centers)]
                                       #fill = as.factor(1:range(nrow(kp$centers)))
                         ),
                         label = display_age_range)+
        theme(legend.position="none")+
        ggtitle(paste0("Dimensions: ",paste(names(test2)[1:(length(test2)-2)],collapse = ",")))
      
      
      cluster_center <- kp$centers
      
      print(list(cluster_plot,cluster_center,kp$lambda)) 
    }
  }
  
}