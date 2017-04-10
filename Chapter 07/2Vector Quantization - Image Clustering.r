R version 3.3.2 (2016-10-31) -- "Sincere Pumpkin Patch"
Copyright (C) 2016 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R

 library(jpeg)
 library(ggplot2)

 img <- readJPEG("d:/Image.jpg")
 
 img_Dim <- dim(img)
 
 img_Dim
[1] 526 800   3
 

 
 img_RGB_channels  <- data.frame(
     x = rep(1:img_Dim[2], each = img_Dim[1]),
     y = rep(img_Dim[1]:1, img_Dim[2]),
     R = as.vector(img[,,1]),
     G = as.vector(img[,,2]),
     B = as.vector(img[,,3])
 )
 
 
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


 
 ggplot(data = img_RGB_channels, aes(x = x, y = y)) + 
     geom_point(colour = rgb(img_RGB_channels[c("R", "G", "B")])) +
     labs(title = "Original Image: Colorful Bird") +
     xlab("x") +
     ylab("y") +
     plotTheme()
 
 kClusters <- 3
 
 kMeans_clst <- kmeans(img_RGB_channels[, c("R", "G", "B")], centers = kClusters)
 
 kColours <- rgb(kMeans_clst$centers[kMeans_clst$cluster,])
 
 ggplot(data = img_RGB_channels, aes(x = x, y = y)) + 
     geom_point(colour = kColours) +
     labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
     xlab("x") +
     ylab("y") + 
     plotTheme()

 kClusters <- 5

 kMeans_clst <- kmeans(img_RGB_channels[, c("R", "G", "B")], centers = kClusters)
 
 kColours <- rgb(kMeans_clst$centers[kMeans_clst$cluster,])
     
 ggplot(data = img_RGB_channels, aes(x = x, y = y)) + 
     geom_point(colour = kColours) +
     labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
     xlab("x") +
     ylab("y") + 
     plotTheme()

 
 ggplot(data = img_RGB_channels, aes(x = x, y = y)) + 
     geom_point(colour = rgb(img_RGB_channels[c("R", "G", "B")])) +
     labs(title = "Original Image: Colorful Bird") +
     xlab("x") +
     ylab("y") +
     plotTheme()
 
 kClusters <- 3
 
 kMeans_clst <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
 
 kColours <- rgb(kMeans_clst$centers[kMeans_clst$cluster,])
 
 ggplot(data = imgRGB, aes(x = x, y = y)) + 
     geom_point(colour = kColours) +
     labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
     xlab("x") +
     ylab("y") + 
     plotTheme()

 kClusters <- 5

 kMeans_clst <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
 
 kColours <- rgb(kMeans_clst$centers[kMeans_clst$cluster,])
     
 ggplot(data = imgRGB, aes(x = x, y = y)) + 
     geom_point(colour = kColours) +
     labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
     xlab("x") +
     ylab("y") + 
     plotTheme()
     
 