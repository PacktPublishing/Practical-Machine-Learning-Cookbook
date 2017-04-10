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

[Workspace loaded from ~/.RData]

 install.packages("gdata")
 install.packages("evir")
 library(gdata)
 library(evir)

 StormDamageData <- read.xls("d:/publicdatamay2007.xls", sheet = 1)
 
 head(StormDamageData)
 
 tail(StormDamageData)
 
 dim(StormDamageData)
 
 ChangeFormat <- function(x){
                     x=as.character(x)
                     for(i in 1:10){x=sub(",","",as.character(x))}
                     return(as.numeric(x))}
 
 base <- StormDamageData[,1:4]

 base$Base.Economic.Damage <- Vectorize(ChangeFormat)(StormDamageData$Base.Economic.Damage)
 
 base$Normalized.PL05 <- Vectorize(ChangeFormat)(StormDamageData$Normalized.PL05)

 base$Normalized.CL05 <- Vectorize(ChangeFormat)(StormDamageData$Normalized.CL05)

 head(base)

 plot(base$Normalized.PL05/1e9,type="h",ylim=c(0,155), main = "207 Hurricanes, Normalized Costs: 1900 - 2005", xlab = "Index of Loss", ylab = "Normalized Costs", col = "red")
 
 TestBase <- table(base$Year)

 TestBase

 years <- as.numeric(names(TestBase))

 years

 frequency <- as.numeric(TestBase)

 frequency

 years0frequency <- (1900:2005)[which(!(1900:2005)%in%years)]
 
 years0frequency

 StormDamageData <- data.frame(years=c(years, years0frequency), frequency=c(frequency, rep(0,length(years0frequency))))
 
 head(StormDamageData)

 plot(years, frequency, type="h", main = "Frequency of Hurricanes: 1900 - 2005", xlab = "Time (Years)", ylab = "Annual Frequency", col = "red")
 
 mean(StormDamageData$frequency)

 LinearTrend <- glm(frequency~years, data = StormDamageData, family=poisson(link="identity"), start=lm(frequency~years, data = StormDamageData)$coefficients)

 LinearTrend

 ExpTrend <- glm(frequency~years, data=StormDamageData, family = poisson(link="log"))

 ExpTrend

 plot(years, frequency, type='h', ylim=c(0,6), main = "No. of Major Hurricanes Predicted for 2014", xlim=c(1900,2020))

 cpred1 <- predict(ExpTrend, newdata = data.frame(years=1890:2030), type="response")

 cpred1
 
 lines(1890:2030,cpred1,col="blue")

 cpred0 <- predict(LinearTrend, newdata=data.frame(years=1890:2030), type="response")

 cpred0
 
 lines(1890:2030, cpred0, col="red")
 
 abline(h = mean(StormDamageData$frequency), col="black")

 predictions <- cbind(constant = mean(StormDamageData$frequency), linear= cpred0[126], exponential=cpred1[126])

 predictions

 points(rep((1890:2030)[126],3), predictions, col=c("black","red","blue"), pch=19)

 hill(base$Normalized.PL05)

 threshold <- .5
 
 gpd.PL <- gpd(base$Normalized.PL05/1e9/20, threshold)$par.ests

 mean(base$Normalized.CL05/1e9/20 >.5)
 
 ExpectedValue <- function(yinf,ysup,xi,beta){
    as.numeric(integrate(function(x) (x-yinf) * dgpd(x,xi,mu=threshold,beta),
    lower=yinf,upper=ysup)$value +
    (1-pgpd(ysup,xi,mu=threshold,beta))*(ysup-yinf))
     }

 predictions[1]

 mean(base$Normalized.PL05/1e9/20.5)

 ExpectedValue(2,6,gpd.PL[1],gpd.PL[2])*1e3
 
 predictions[1] * mean(base$Normalized.PL05/1e9/20 >.5) * ExpectedValue(2, 6, gpd.PL[1], gpd.PL[2]) * 1e3
