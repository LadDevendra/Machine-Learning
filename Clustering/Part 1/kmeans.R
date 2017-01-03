args = commandArgs(trailingOnly=TRUE)

if (length(args) != 3) {
  
  stop("kmeans.R <number_of_clusters> <test_datafile> <output_file>", call.=FALSE)
} else{
  
# Function for calculating euclidean distance
eucl_distance <- function(a,b){
  
  return(sqrt((a^2 + b^2)))
}

# calculating the sum of squared errors
sum_sqr_err <- function(result){
  
  sse <- 0
  for(i in 1:k){
    
    data_pts <- which(result$data$V3 %in% i)
    var <- 0
    for(point in data_pts){
      dist <- eucl_distance(result$centroid[i,1]-data[point,1]
                            ,result$centroid[i,2]-data[point,2])
      var <- var + (dist ^ 2)
    }
    sse <- sse + var
  }
  return(sse)
}

  
# kmean algorithm
k_means <- function(k,max_itr){
    
  # getting the minimum and maximum values of x and y
  min_x = min(data$x)
  min_y = min(data$y)
  max_x = max(data$x)
  max_y = max(data$y)
    
  # generating centroids in random between the interval (min, max)
  centroid <- c()
  for(i in 1:k){

    centroid <- rbind(centroid,c(round(runif(1,min = min_x, max = max_x),3)
                           ,round(runif(1,min = min_y, max = max_y),3)))
  }
  
  
  # initializing the 3rd column to store the centroid to which 
  # instance belongs
  data[1,3] <- 0
  
  # binding the 3rd column to help design the break condition
  centroid <- cbind(centroid,0)
  
  # calculating the centroids again
  recal_centroids <- function(){
  
    n_centroids <- c()
    for(i in 1:k){
      data_pts <- which(data[,3] %in% i)
        
      if(length(data_pts) != 0){
        x_value <- 0
        y_value <- 0
        for(point in data_pts){
          x_value <- x_value + data[point,1]
          y_value <- y_value + data[point,2]
        }
        n_centroids <- rbind(n_centroids,c(round(x_value/length(data_pts),3)
                                             , round(y_value/length(data_pts),3)))
      }
      else{
        n_centroids <- rbind(n_centroids, c(centroid[i,1],centroid[i,2]))
      }
    }
    return(n_centroids)
  }# end of recal_centroids() function
  
  # repeating the iterations   
  itr <- 0
  repeat{
    
    itr <- itr + 1
    # getting the euclidean distances between initial centroids, data points
    for(i in 1:nrow(data)){
      
      euc_distance <- c()
      for(j in 1:k){
        euc_distance[j] <- round(eucl_distance(data[i,1]-centroid[j,1] , data[i,2] - centroid[j,2]),3)
      }
      
      # matching min of the euclidean distance to a k'th centroid
      # centroid saved corresponding to the instances
      data[i,3] <- match(min(euc_distance),euc_distance)
    }
      
    # recaluculating the centroids
    new_cents <- recal_centroids()
    
    # updating the centroids after recalculating the centroids 
    for(i in 1:k){
      if( (centroid[i,1]!=new_cents[i,1]) || (centroid[i,2]!=new_cents[i,2]) ){
        centroid[i,1] = new_cents[i,1]
        centroid[i,2] = new_cents[i,2]
        centroid[i,3] <- 1
      }
    }
    
    # No change in centroids leads to centroid[i,3] => 0. Exit condition
    num_centroids = length(which(centroid[,3] %in% 0))
    if( (num_centroids == k) || itr == 25) break
    
    centroid[,3] <- 0
  }
  
  print("Final Centroids points:")
  print(centroid)
  print(paste("Iterations taken:",itr))
  
  # initializing result vector  
  result <- c()
  result$data <- data
  result$centroid <- centroid
  return(result)
}
  
  
# Taking arguments from the commandLine
# k => number of centroids
# max_itr => maximum number of iterations allowed  
k <- as.integer(args[1])
max_itr = 25
  
# reading the data from test_data file
data <- read.table(args[2], header=TRUE)

# removing the index column id from test_data file
data <- data[-1]
  
# calling the k_means algorithm's function call
result <- k_means(k,max_itr)

# getting the sum of squared errors  
sse <- sum_sqr_err(result)
  

# initializing final result vector
final_result <- c()
  
for(i in 1:k){
  final_result <- rbind(final_result,c(i,
                                 paste(which(result$data[,3] %in% i), collapse = ",")))
}
  
final_result <- rbind(final_result,c("SSE", sse))
data_frame<-  data.frame(All_centroids = final_result[,1], cluster_points = final_result[,2])

# writing the final results to the text file
write.table(data_frame,args[3],row.names = FALSE, col.names = FALSE, sep = " ")
}
