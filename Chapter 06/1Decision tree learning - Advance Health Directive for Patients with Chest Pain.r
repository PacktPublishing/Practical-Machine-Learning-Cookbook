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
 install.packages("tree")
 install.packages("caret")
 install.packages("e1071")
 library(tree)
 library(caret)

 AHD_data <- read.csv("d:/Heart.csv", header = TRUE)

 str(AHD_data)

 head(AHD_data)

 dim(AHD_data)

 split <- createDataPartition(y=AHD_data$AHD, p = 0.5, list=FALSE)
 
 split

 train <- AHD_data[split,]
 
 train

 test <- AHD_data[-split,]
 
 test

 trees <- tree(AHD ~., train)
 
 plot(trees)

 cv.trees <- cv.tree(trees, FUN=prune.misclass)

 cv.trees

 plot(cv.trees)
 
 prune.trees <- prune.misclass(trees, best=4)

 plot(prune.trees)
 
 text(prune.trees, pretty=0)
 
 tree.pred <- predict(prune.trees, test, type='class')
 
 tree.pred

 confusionMatrix(tree.pred, test$AHD)
