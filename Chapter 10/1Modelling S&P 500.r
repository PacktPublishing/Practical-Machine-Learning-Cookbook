
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
Type 'q()' to quit R.

 install.packages("quantmod")
 install.packages("neuralnet")
 library(quantmod)
 library(neuralnet)

 startDate = as.Date("2009-01-01")

 endDate = as.Date("2014-01-01") 
 
 getSymbols("^GSPC",src="yahoo",from=startDate,to=endDate) 

 relativeStrengthIndex3 <- RSI(Op(GSPC),n=3)

 summary(relativeStrengthIndex3)

 exponentialMovingAverage5 <- EMA(Op(GSPC),n=5)
 
 
 head(exponentialMovingAverage5)

 summary(exponentialMovingAverage5)

 exponentialMovingAverageDiff <- Op(GSPC) - exponentialMovingAverage5
 
 head(exponentialMovingAverageDiff)
 
 summary(exponentialMovingAverageDiff)

 
 MACD <- MACD(Op(GSPC),fast = 12, slow = 26, signal = 9)

 tail(MACD)

 
 summary(MACD)

 MACDsignal <- MACD[,2]

 BollingerBands <- BBands(Op(GSPC),n=20,sd=2)
 
 tail(BollingerBands)

 summary(BollingerBands)
 
 PercentageChngpctB <- BollingerBands[,4]


 
 tail(PercentageChngpctB)

 summary(PercentageChngpctB)

 
 Price <- Cl(GSPC)-Op(GSPC)
 
 tail(Price)

 DataSet<-data.frame(relativeStrengthIndex3, exponentialMovingAverage5, MACDsignal, PercentageChngpctB, Price)
 
 str(DataSet)

 DataSet<-DataSet[-c(1:33),]
 
 dim(DataSet)

 colnames(DataSet)<-c("RSI3","EMAcross","MACDsignal","BollingerB","Price")
 
 str(DataSet)

 Normalized <-function(x) {(x-min(x))/(max(x)-min(x))}

 NormalizedData<-as.data.frame(lapply(DataSet,Normalized))
 
 tail(NormalizedData)

 TrainingSet<-NormalizedData[1:816,]
 
 dim(TrainingSet)

 summary(TrainingSet)


 TestSet<-NormalizedData[817:1225 ,]
 
 dim(TestSet)

 summary(TestSet)

 nn1<-neuralnet(Price~RSI3+EMAcross+MACDsignal+BollingerB,data=TrainingSet, hidden=c(3,3), learningrate=.001,algorithm="backprop")


plot(nn1)