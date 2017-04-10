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
 install.packages("randomForest")
 library(randomForest)
 install.packages("Hmisc")
 library(describe)

 PoundDollar <- read.csv("d:/PoundDollar.csv")
 
 head(PoundDollar)
 
 summary(PoundDollar)

 dim(PoundDollar)

 DateAndTime <- as.POSIXlt(PoundDollar[,2],format="%m/%d/%y %H:%M")

 HighLowClose <- PoundDollar[,4:6]

 head(HighLowClose)

 summary(HighLowClose)

 str(HighLowClose)
 
 HighLowClosets <- data.frame(HighLowClose,row.names=DateAndTime)

 describe(HighLowClosets)
 
 HighLowClosexts <- as.xts(HighLowClosets)

 BollingerBands <- BBands(HighLowClosexts,n=20,SMA,sd=2)

 describe(BollingerBands)

 Upper <- BollingerBands$up - HighLowClosexts$Close

 summary(Upper)
 
 Lower <- BollingerBands$dn - HighLowClosexts$Close

 summary(Lower)
 
 Middle <- BollingerBands$mavg - HighLowClosexts$Close

 summary(Middle)

 PercentageChngpctB <- Delt(BollingerBands$pctB,k=1)

 describe(PercentageChngpctB)

 PercentageChngUp <-Delt(Upper,k=1)

 describe(PercentageChngUp)

 PercentageChngLow <- Delt(Lower,k=1)
 
 describe(PercentageChngLow)
 
 PercentageChngMid <- Delt(Middle,k=1)
 
 describe(PercentageChngMid)

 Returns <- Delt(HighLowClosexts$Close,k=1)

 binaryClassification <- ifelse(Returns>0,"Up","Down")


 summary(binaryClassification)

 ClassShifted <- binaryClassification[-1]
 
 FeaturesCombined <- data.frame(Upper, Lower, Middle, BollingerBands$pctB, PercentageChngpctB, PercentageChngUp, PercentageChngLow, PercentageChngMid)    

 summary(FeaturesCombined)

 FeaturesShifted <- FeaturesCombined[-5257,]
 
 FeaturesClassData <- data.frame(FeaturesShifted,ClassShifted) 

 summary(FeaturesClassData)
 
 FinalModelData <- FeaturesClassData[-c(1:20),]
 
 colnames(FinalModelData) <- c("pctB","LowDiff","UpDiff","MidDiff","PercentageChngpctB","PercentageChngUp","PercentageChngLow","PercentageChngMid","binaryClassification") 
 
 str(FinalModelData)
 
 set.seed(1)
 
 FeatureNumber <- tuneRF(FinalModelData[,-9],FinalModelData[,9],ntreeTry=100, stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)
 
 RandomForest <- randomForest(binaryClassification~.,data=FinalModelData,mtry=2,ntree=2000,keep.forest=TRUE,importance=TRUE)
 
 varImpPlot(RandomForest, main = 'Random Forest: Measurement of Importance of Each Feature',pch=16,col='blue' )




 