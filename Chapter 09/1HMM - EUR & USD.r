R version 3.2.2 (2015-08-14) -- "Fire Safety"
Copyright (C) 2015 The R Foundation for Statistical Computing
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
 install.packages("depmixS4")
 library(depmixS4)
 install.packages("quantmod")
 library(quantmod)
 install.packages("ggplot2")
 library(ggplot2)


 EuroUSD <- read.csv("d:/EURUSD1d.csv", header = TRUE)

 head(EuroUSD)


 summary(EuroUSD)


 str(EuroUSD)

 Date <- as.character(EuroUSD[,1])

 DateTimeSeries <- as.POSIXlt(Date, format = "%Y.%m.%d %H:%M:%S")

 TimeSeriesData <- data.frame(EuroUSD[,2:5], row.names = DateTimeSeries)


 head(TimeSeriesData)


 TimeSeriesData <-as.xts(TimeSeriesData)

 ATRindicator <- ATR(TimeSeriesData[,2:4],n=14)

 head(ATRindicator)


 TrueRange <- ATRindicator[,2]
 
 head(TrueRange)

 
 LogReturns <- log(EuroUSD$Close) - log(EuroUSD$Open)
 
 summary(LogReturns)

 HMMModel <- data.frame(LogReturns, TrueRange)

 HMMModel <- HMMModel[-c(1:14),]

 head(HMMModel)
 
 colnames(HMMModel) <- c("LogReturns","TrueRange")


 colnames(HMMModel)
 
 set.seed(1)
 
 HMM <- depmix(list(LogReturns~1, TrueRange~1), data = HMMModel, nstates=3, family=list(gaussian(), gaussian()))

 HMMfit <- fit(HMM, verbose = FALSE)

 
 print(HMMfit)

 summary(HMMfit)
 
 HMMstate <- posterior(HMMfit)
 
 head(HMMstate)

 
 DFIndicators <- data.frame(DateTimeSeries, LogReturns, TrueRange)

 DFIndicatorsClean <- DFIndicators[-c(1:14), ]

 Plot1Data <- data.frame(DFIndicatorsClean, HMMstate$state)

 LogReturnsPlot <- ggplot(Plot1Data,aes(x=Plot1Data[,1],y=Plot1Data[,2]))+geom_line(color="darkred")+labs(title="Euro USD Daily Log Returns",y="Log Return Values",x="Date")
 LogReturnsPlot
 