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

 install.packages("quantmod")
 library(quantmod)
 install.packages("lubridate")
 library(lubridate)
 install.packages("e1071")
 library(e1071)
 
 startDate = as.Date("2012-01-01")
 
 endDate = as.Date("2014-01-01") 
 
 getSymbols("AAPL", env = .GlobalEnv, src = "yahoo", from = startDate, to = endDate) 
 
 weekDays <- wday(AAPL, label=TRUE)
 
 head(weekDays)
 
 changeInPrices <- Cl(AAPL) - Op(AAPL)
 
 head(changeInPrices)
 
 summary(changeInPrices)
 
 dim(changeInPrices)
 
 binaryClassification <- ifelse(changeInPrices>0,"UP","DOWN")
 
 binaryClassification
 
 summary(binaryClassification)
 
 AAPLDataSet <- data.frame(weekDays,binaryClassification)
 
 AAPLDataSet
 
 head(AAPLDataSet)
 
 dim(AAPLDataSet)
 
 NaiveBayesclassifier <- naiveBayes(AAPLDataSet[,1], AAPLDataSet[,2])
 
 NaiveBayesclassifier
 
 exponentialMovingAverage5 <- EMA(Op(AAPL),n = 5)
 
 exponentialMovingAverage5
 
 summary(exponentialMovingAverage5)

 exponentialMovingAverage10 <-EMA(Op(AAPL),n = 10)

 exponentialMovingAverage10
 
 summary(exponentialMovingAverage10)
 
 dim(exponentialMovingAverage10)

 exponentialMovingAverageDiff <- exponentialMovingAverage5 - exponentialMovingAverage10 
 
 exponentialMovingAverageDiff
 
 summary(exponentialMovingAverageDiff)
 
 exponentialMovingAverageDiffRound <- round(exponentialMovingAverageDiff, 2)

 summary(exponentialMovingAverageDiffRound)

 AAPLDataSetNew <- data.frame(weekDays,exponentialMovingAverageDiffRound, binaryClassification)
 
 AAPLDataSetNew
 
 summary(AAPLDataSetNew)
 
 AAPLDataSetNew <- AAPLDataSetNew[-c(1:10),]

 AAPLDataSetNew
 
 summary(AAPLDataSetNew)
 
 dim(AAPLDataSetNew)

 trainingDataSet <- AAPLDataSetNew[1:328,]
 
 dim(trainingDataSet)
 
 summary(trainingDataSet)

 TestDataSet <- AAPLDataSetNew[329:492,] 
 
 dim(TestDataSet)
 
 summary(TestDataSet)
 
 exponentialMovingAverageDiffRoundModel <- naiveBayes(trainingDataSet[,1:2],trainingDataSet[,3])
 
 exponentialMovingAverageDiffRoundModel
 
 table(predict(exponentialMovingAverageDiffRoundModel,TestDataSet),TestDataSet[,3],dnn=list('Predicted','Actual'))