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
 library("quantmod")

 install.packages("rpart")
 library("rpart")

 install.packages("rpart.plot")
 library("rpart.plot")


 startDate = as.Date("2012-01-01")
 
 endDate = as.Date("2014-01-01")
 
 getSymbols("BAC", env = .GlobalEnv, src = "yahoo", from = startDate, to = endDate) 
 
 relativeStrengthIndex3<-RSI(Op(BAC), n= 3)
 
 relativeStrengthIndex3
 
 exponentialMovingAverage5<-EMA(Op(BAC),n=5)
 
 exponentialMovingAverage5

 dim(exponentialMovingAverage5)

 str(exponentialMovingAverage5)

 exponentialMovingAverageDiff<- Op(BAC)-exponentialMovingAverage5

 exponentialMovingAverageDiff

 MACD<-MACD(Op(BAC),fast = 12, slow = 26, signal = 9)
 
 MACD

 head(MACD)

 MACDsignal<-MACD[,2]

 MACDsignal

 stochasticOscillator <- SMI(Op(BAC),n=13,slow=25,fast=2,signal=9) 


 stochasticOscillator


 stochasticOscillatorSignal <- stochasticOscillator[,1]
 
 stochasticOscillatorSignal


 PriceChange <- Cl(BAC) - Op(BAC)

 PriceChange
 
 binaryClassification <- ifelse(PriceChange>0,"UP","DOWN")
 
 binaryClassification
 
 str(binaryClassification)


 DataSet<-data.frame(relativeStrengthIndex3, exponentialMovingAverageDiff, MACDsignal, stochasticOscillator, binaryClassification)

 DataSet


 head(DataSet)

 str(DataSet)

 colnames(DataSet) <- c("relativeStrengthIndex3", "exponentialMovingAverageDiff", "MACDsignal", "stochasticOscillator", "binaryClassification") 


 colnames(DataSet)
 
 DataSet<-DataSet[-c(1:33),]
 
 
 DataSet


 head(DataSet)

 str(DataSet)

 dim(DataSet)

 TrainingDataSet <- DataSet[1:312,]
 TrainingDataSet

 str(TrainingDataSet)

 TestDataSet <- DataSet[313:469,]

 TestDataSet

 str(TestDataSet)
 
 DecisionTree<-rpart(binaryClassification~relativeStrengthIndex3+exponentialMovingAverageDiff+MACDsignal+stochasticOscillator,data=TrainingDataSet, cp=.001)


 prp(DecisionTree,type=2)
 
 printcp(DecisionTree)

 plotcp(DecisionTree,upper="splits")

 PrunedDecisionTree<-prune(DecisionTree,cp=0.041428)

 prp(PrunedDecisionTree, type=4)
 
 table(predict(PrunedDecisionTree,TestDataSet),TestDataSet[,5],dnn=list('predicted','actual'))
