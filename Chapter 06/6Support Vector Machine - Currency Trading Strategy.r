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

 install.packages("quantmod")
 library(quantmod)
 install.packages("e1071")
 library(e1071)
 install.packages("ggplot2")
 install.packages("Hmisc")
 library(ggplot2)

 library(Hmisc)

 PoundDollar <- read.csv("d:/PoundDollar.csv")

 head(PoundDollar)
 
 str(PoundDollar)

 relativeStrengthIndex3 <- RSI(Op(PoundDollar), n= 3)

 summary(relativeStrengthIndex3)

 SeriesMeanAvg50 <- SMA(Op(PoundDollar),n=50)
 
 summary(SeriesMeanAvg50)
 
 describe(SeriesMeanAvg50)

 Trend <- Op(PoundDollar) - SeriesMeanAvg50
 
 summary(Trend)
 
 PriceDiff <- Cl(PoundDollar) - Op(PoundDollar)
 
 summary(PriceDiff)

 binaryClassification <- ifelse(PriceDiff>0,"UP","DOWN")
 
 summary(binaryClassification)

 DataSet <- data.frame(relativeStrengthIndex3, Trend, binaryClassification)

 str(DataSet)
 
 DataSet<-DataSet[-c(1:49),]
 
 dim(DataSet)

 TrainingDataSet <- DataSet[1:4528,]
 
 dim(TrainingDataSet)
 
 summary(TrainingDataSet)
 
 TestDataSet <- DataSet[4529:6038,]
 
 dim(TestDataSet)

 summary(TestDataSet)
 
 SVM <- svm(binaryClassification~relativeStrengthIndex3+Trend, data=TrainingDataSet, kernel="radial",cost=1,gamma=1/2)

 summary(SVM)

 TrainingPredictions <- predict(SVM,TrainingDataSet, type="class")
 
 summary(TrainingPredictions)
 
 describe(TrainingPredictions)
 
 TrainingData <- data.frame(TrainingDataSet, TrainingPredictions)
 
 summary(TrainingData)
 
 ggplot(TrainingData,aes(x=Trend,y=relativeStrengthIndex3))+stat_density2d(geom="contour",aes(color=TrainingPredictions))+labs(title="SVM Relative Strength Index & Trend Predictions",x="Open - SMA50",y="RSI3",color="Training Predictions")