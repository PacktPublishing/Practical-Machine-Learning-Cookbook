
R version 3.3.0 (2016-05-03) -- "Supposedly Educational"
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
Type 'q()' to quit R.

[Workspace loaded from ~/.RData]

 library(tree)

 realEstate <- read.table("d:/RealEstate.txt", header=TRUE)

 dim(realEstate)

 str(realEstate)
 
 head(realEstate)

 summary(realEstate)

 treeModel <- tree(log(MedianHouseValue) ~ Longitude + Latitude, data=realEstate)
 
 summary(treeModel)

 plot(treeModel)
 
 text(treeModel, cex=.75)

 priceDeciles <- quantile(realEstate$MedianHouseValue, 0:10/10)
 
 priceDeciles
 
 summary(priceDeciles)
  
 cutPrices <- cut(realEstate$MedianHouseValue, priceDeciles, include.lowest=TRUE)

 head(cutPrices)

 summary(cutPrices)

 plot(realEstate$Longitude, realEstate$Latitude, col=grey(10:2/11)[cutPrices], pch=20, xlab="Longitude",ylab="Latitude")

 summary(realEstate$Longitude)

 head(realEstate$Longitude)

 summary( realEstate$Latitude)

 head( realEstate$Latitude)
 
 partition.tree(treeModel, ordvars=c("Longitude","Latitude"), add=TRUE)
 
 treeModel2 <- tree(log(MedianHouseValue) ~ Longitude + Latitude, data=realEstate, mindev=0.001)
 
 summary(treeModel2)
 
 plot(treeModel2)
  
 text(treeModel2, cex=.65)

 treeModel3 <- tree(log(MedianHouseValue) ~ ., data=realEstate)
 
 summary(treeModel3)
 
 plot(treeModel3)

 text(treeModel3, cex=.75)