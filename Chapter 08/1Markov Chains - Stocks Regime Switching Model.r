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

[Workspace loaded from ~/.RData]

 install.packages("MSwM")

 library(MSwM)

 MarkovSwitchData <- read.csv("d:/StocksRegimeSwitching.csv", header = TRUE)

 attach(MarkovSwitchData)

 head(MarkovSwitchData)
 
 dim(MarkovSwitchData)
 
 summary(MarkovSwitchData)
 
 yLogValueStocks <- cbind(LVS)

 head(yLogValueStocks)

 yLogGrowthStocks <- cbind(LGS)
 
 head(yLogGrowthStocks)
 
 x <- cbind(LRY, LRC, INT, LRV)
 
 olsLogValueStocks <- lm(yLogValueStocks~x)
 
 summary(olsLogValueStocks)
 
 olsLogGrowthStocks <- lm(yLogGrowthStocks~x)
 
 summary(olsLogGrowthStocks)
 
 MarkovSwtchLogValueStocks <- msmFit(olsLogValueStocks, k = 2, sw = rep(TRUE, 6))
 
 summary(MarkovSwtchLogValueStocks)
 
 MarkoSwtchLogGrowthStocks <- msmFit(olsLogGrowthStocks, k = 2, sw = rep(TRUE, 6))
 
 summary(MarkoSwtchLogGrowthStocks)
 
 par(mar=c(3,3,3,3))
 
 plotProb(MarkovSwtchLogValueStocks, which=1)
  
 plotProb(MarkoSwtchLogValueStocks, which=2)
  
 plotProb(MarkoSwtchLogGrowthStocks, which=1)
  
 plotProb(MarkoSwtchLogGrowthStocks, which=2)

 par(mar=c(3,3,3,3))
 
 plotDiag(MarkovSwtchLogValueStocks, regime=1, which=1)
  
 plotDiag(MarkovSwtchLogValueStocks, regime=1, which=2)
 
 plotDiag(MarkoSwtchLogGrowthStocks, regime=1, which=1)
 
 plotDiag(MarkoSwtchLogGrowthStocks, regime=1, which=2)
 
 plotDiag(MarkoSwtchLogGrowthStocks, regime=1, which=3)

