library(jpeg)
library(ggplot2)

#jpeg package to Read the image
img <- readJPEG("image1.jpg") 

# Obtain the dimension
imgDm <- dim(img)

# Assign RGB channels to data frame
imgRGB <- data.frame(
  x = rep(1:imgDm[2], each = imgDm[1]),
  y = rep(imgDm[1]:1, imgDm[2]),
  R = as.vector(img[,,1]),
  G = as.vector(img[,,2]),
  B = as.vector(img[,,3])
)

# ggplot theme to be used
plotTheme <- function() {
  theme(
    panel.background = element_rect(
      size = 3,
      colour = "black",
      fill = "white"),
    axis.ticks = element_line(
      size = 2),
    panel.grid.major = element_line(
      colour = "gray80",
      linetype = "dotted"),
    panel.grid.minor = element_line(
      colour = "gray90",
      linetype = "dashed"),
    axis.title.x = element_text(
      size = rel(1.2),
      face = "bold"),
    axis.title.y = element_text(
      size = rel(1.2),
      face = "bold"),
    plot.title = element_text(
      size = 20,
      face = "bold",
      vjust = 1.5)
  )
}

# Plot the original image
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +
  labs(title = "Original Image") +
  xlab("x") +
  ylab("y") +
  plotTheme()

# K-Means Clustering #

kClusters <- 3
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters,trace=TRUE)
kColours <- rgb(kMeans$centers[kMeans$cluster,])

ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y") + 
  plotTheme()

# Plotting the Voronoi cells #

# Number of Voronoi cells.
nRegions <- 2000 
voronoiMeans <- kmeans(imgRGB, centers = nRegions, iter.max = 50)
voronoiColor <- rgb(voronoiMeans$centers[voronoiMeans$cluster, 3:5])

plot(y ~ x, data=imgRGB, col = voronoiColor, 
     asp = 1, pch = ".", main="Voronoi cells",
     axes=FALSE, ylab="", xlab="2000 local clusters")

# Plotting cluster centers as little rectangles #

nRegions <- 2000  
voronoiMeans <- kmeans(imgRGB, centers = nRegions, iter.max = 50)
voronoiColor <- rgb(voronoiMeans$centers[,3:5])

plot(y ~ x, data=voronoiMeans$centers, 
     col = voronoiColor, 
     main="Cluster centers as rectangles", asp = 1, 
     pch = 15, cex=1.5, axes=FALSE, 
     ylab="", xlab="2000 local clusters")