args = commandArgs(trailingOnly=TRUE)
options(warn=-1)

if (length(args) != 4) {
  
  stop("tweets_kmeans.R <number_of_clusters> <initial_seeds_textfile> <tweets_jsonfile> <output_file>", call.=FALSE)  
}else{

  if (!require("stringr")) {
    install.packages("stringr", repos="http://cran.rstudio.com/") 
    library("stringr")
  }
  
  if (!require("jsonlite")) {
    install.packages("jsonlite", repos="http://cran.rstudio.com/") 
    library("jsonlite")
  }
  
  # function returns the jaccard distance
  jac_distance <- function(s1,s2){
    
    set_1 <- cln_data(s1)
    set_2 <- cln_data(s2)
    
    if(length(set_1) == 0 || length(set_2)==0){
      x_value <- 0
      y_value <- 0
    }
    else{
      x_value <- length(intersect(set_1,set_2))
      y_value <- length(union(set_1,set_2))
    }
    
    result <- round(1-(x_value/y_value),3)
    return(result)
  }
  
  
  # cleaning twitter data
  cln_data <- function(str){
    
    var <- toupper(str)
    var <- stringr::str_replace_all(var,"[^A-Z0-9\\s]", " ")
    var <- stringr::str_replace_all(var,"[\\t]", " ")
    var <- stringr::str_replace_all(var,"[\\s]", " ")
    var <- stringr::str_split(var, " ")[[1]]
    return(var)
  }
  
  # calculating the sum of squared errors  
  sum_sqr_err <- function(){
    
    sse <- 0
    for(i in 1:k){
      s1 <- tweets[which(tweets[,"id_str"] %in% centroids[i,1]),"text"]
      data_pts <- which(tweets[,"clus_id"] %in% i)
      if(length(data_pts)!=0){
        for(point in data_pts){
          s2 <- tweets[point,"text"]
          jac_dist <- jac_distance(s1,s2)
          
          sse <- sse+(jac_dist ^ 2)
        }
      }
    }
    return(sse)
  }
  
  
  # kmeans algorithm using Jaccards distance
  kmeans <- function(){
    
    itr <- 0
    repeat{
      itr <- itr+1
      
      # taking the initial centroids
      for(i in 1:nrow(tweets)){
        s1 <- tweets[i,"text"]
        jac_dist <- c()
        
        for(j in 1:k){
          s2 <- tweets[which(tweets[,"id_str"] %in% centroids[j,1]),"text"]
          jac_dist[j] <- jac_distance(s1,s2)
        }
        
        # clustering tweet IDs according to the initial centroids
        tweets[i,"clus_id"] <- match(min(jac_dist),jac_dist)
      }
      
      # calculating new centroids
      new_cents <- c()
      for(i in 1:k){
        jac_dist <- c()
        data_pts <- which(tweets[,"clus_id"] %in% i)
        
        if(length(data_pts)!=0){
          for(j in 1:length(data_pts)){
            s1 <- tweets[data_pts[j],"text"]
            
            # considering other points than s1
            other_points <- data_pts[-j]
            var <- 0
            for(point in other_points){
              s2 <- tweets[point,"text"]
              var <- var + jac_distance(s1,s2)
            }
            jac_dist[j] <- var
            
          }
          new_cents[i] <- tweets[data_pts[match(min(jac_dist),jac_dist)], "id_str"]
        }
        else{
          new_cents[i] <- centroids[i,1]
        }
      }
      
      for(i in 1:k){
        if(new_cents[i] != centroids[i,1]){
          centroids[i,1] <- new_cents[i]
          centroids[i,2] <- 1
        }
      }
      
      # breaking condition after storing all the tweet ids
      if(length(which(centroids[i,2] %in% 1)) == 0) break
      centroids[i,2] <- 0
    }
    
    return(tweets)
  }
  
  
  # taking the value of nUmber of clusters
  k <- as.integer(args[1])
  
  # reading InitialSeeds.txt file
  given_centroids <- read.table(args[2], header = FALSE)
  
  centroids <- c()
  for(i in 1:length(given_centroids$V1)){
    centroids <- rbind(centroids,str_replace_all(given_centroids$V1[i],",",""))
  }
  centroids <- cbind(centroids,0)
  
  # reading twitter data
  tweets <- stream_in(file(args[3]))
  
  # initializing the cluster_ID column => clus_id
  tweets["clus_id"] <- 0
  
  
  # calling kmeans algorithm
  tweets <- kmeans()
  
  
  # validate clustering
  sse <- sum_sqr_err()
  
  # final result vector 
  final_result <- c()
  
  for(i in 1:k){
    final_result <- rbind(final_result,c(i,
                                         paste(tweets[which(tweets[,"clus_id"] %in% i),"id_str"], collapse = ",")))
  }
  
  final_result <- rbind(final_result,c("SSE", sse))
  data_frame<-  data.frame(All_centroids = final_result[,1], cluster_points = final_result[,2])
  
  # writing the results into the text file
  write.table(data_frame,args[4],row.names = FALSE, col.names = FALSE, sep = " ")
  
  
}

