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

 library(forecast)
 library(lmtest)
 library(caret)

 ud <- read.csv("d:/FRED-WIUR.csv", colClasses=c('Date'='Date'))

 
 tail(ud)

 colnames(ud) <- c('date', 'rate')

 ud$date <- as.Date(ud$date)
 
 summary(ud)

 ud.b <- ud[1:436,]
 
 
 summary(ud.b)

 ud.p <- ud[437:448,]
 
 summary(ud.p)

 ud.ts <- ts(ud.b$rate, start=c(1976, 1), frequency=12)

 ud.ts   

 ud.p.ts<-ts(ud.p$rate, start=c(2012, 5), frequency=12)
 
 ud.p.ts

 plot.ts(ud.ts)

 
 plot.ts(ud.p.ts)

 
 mean <- meanf(ud.ts, 12)

 forecast_randomwalk <- rwf(ud.ts, 12)
 
 forecast_arima <- snaive(ud.ts, 12)
 
 drift <- rwf(ud.ts, 12, drift=T)

 
 m1<-tslm(ud.ts~trend)
 
 m2<-tslm(ud.ts~trend+season)
 
 residual_1 <- residuals(m1)
 
 par(mfrow=c(1,2))

 plot(residual_1, ylab="Residuals",xlab="Year", title("Residual - Trends"), col = "red")
 
 acf(residual_1, main="ACF of residuals")
 
 
 residual_2 <- residuals(m2)
 
 par(mfrow=c(1,2))

 plot(residual_2, ylab="Residuals",xlab="Year",title("Residual - Trends + Seasonality"), col = "red")

 acf(residual_2, main="ACF of residuals")
 
 
 dwtest(m1, alt="two.sided")

 m3 <- stl(ud.ts, s.window='periodic')
 
 plot(m3)
 
 m4<-ets(ud.ts, model='ZZZ')
 
 plot(m4)

 m5 <- auto.arima(ud.ts)
 
 plot(forecast(m5, h=12))
 
 m6<-nnetar(ud.ts)
 
 m6

 plot(forecast(m6, h=12))
 
 
 a1 <- accuracy(mean, ud.p.ts)

 a2 <- accuracy(forecast_randomwalk, ud.p.ts)

 a3<-accuracy(forecast_arima, ud.p.ts)

 a4<-accuracy(drift, ud.p.ts)

 a.table<-rbind(a1, a2, a3, a4)
 
 a.table

 f1<-forecast(m1, h=12)
 
 f2<-forecast(m2, h=12)
 
 f3<-forecast(m3, h=12)
 
 f4<-forecast(m4, h=12)
 
 f5<-forecast(m5, h=12)
 
 f6<-forecast(m6, h=12)

 a5 <- accuracy(f1, ud.p.ts)
 
 a6 <- accuracy(f2, ud.p.ts)
 
 a7 <- accuracy(f3, ud.p.ts)
 
 a8 <- accuracy(f4, ud.p.ts)
 
 a9 <- accuracy(f5, ud.p.ts)
 
 a10 <- accuracy(f6, ud.p.ts)
 
 a.table.1 <- rbind(a5, a6, a7, a8, a9, a10)
 
 a.table.1
