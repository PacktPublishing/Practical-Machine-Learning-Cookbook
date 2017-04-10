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

 install.packages("wbstats")
 install.packages("data.table")
 install.packages("googleVis")

 library(wbstats)
 library(data.table)
 library(googleVis)


 Pop_LifeExp_FertRt <- data.table(wb(indicator = c("SP.POP.TOTL", "SP.DYN.LE00.IN", "SP.DYN.TFRT.IN"), startdate = 1960, enddate = 2016))

 Pop_GDPUSD_HeadCnt <- data.table(wb(indicator = c("SP.POP.TOTL", "NY.GDP.MKTP.CD", "SI.POV.2DAY"), startdate = 1960, enddate = 2016))

 Pop_GDPUSD_Sanitation <- data.table(wb(indicator = c("SP.POP.TOTL", "NY.GDP.MKTP.CD", "SH.STA.ACSN"), startdate = 1960, enddate = 2016))

 GDPUSD_Electricity_CO2 <- data.table(wb(indicator = c("NY.GDP.MKTP.CD", "EG.ELC.ACCS.ZS", "EN.ATM.CO2E.KT"), startdate = 1960, enddate = 2016))

 dim(Pop_LifeExp_FertRt)
 
 dim(Pop_GDPUSD_HeadCnt)

 dim(Pop_GDPUSD_Sanitation)

 dim(GDPUSD_Electricity_CO2)

 str(Pop_LifeExp_FertRt)

 str(Pop_GDPUSD_HeadCnt)

 str(Pop_GDPUSD_Sanitation)

 str(GDPUSD_Electricity_CO2)

 head(Pop_LifeExp_FertRt)

 head(Pop_GDPUSD_HeadCnt)


 head(Pop_GDPUSD_Sanitation)


 
 head(GDPUSD_Electricity_CO2)
 
 dim(wb(indicator = "SP.POP.TOTL"))
 
 dim(wb(indicator = "SP.DYN.LE00.IN"))
 
 dim(wb(indicator = "SP.DYN.TFRT.IN"))
 
 dim(wb(indicator = "NY.GDP.MKTP.CD"))

 dim(wb(indicator = "SI.POV.2DAY"))

 dim(wb(indicator = "SH.STA.ACSN"))

 dim(wb(indicator = "EG.ELC.ACCS.ZS"))

 dim(wb(indicator = "EN.ATM.CO2E.KT"))

 Countries <- data.table(wbcountries())

 head(Countries)

 setkey(Pop_LifeExp_FertRt, iso2c)
 
 setkey(Pop_GDPUSD_HeadCnt, iso2c)
 
 setkey(Pop_GDPUSD_Sanitation, iso2c)
 
 setkey(GDPUSD_Electricity_CO2, iso2c)
 
 
 setkey(Countries, iso2c)
 
 head(setkey(Countries, iso2c))

 Pop_LifeExp_FertRt <- Countries[Pop_LifeExp_FertRt][ ! region %in% "Aggregates"]


 head(Pop_LifeExp_FertRt)

 Pop_GDPUSD_HeadCnt <- Countries[Pop_GDPUSD_HeadCnt][ ! region %in% "Aggregates"]

 Pop_GDPUSD_Sanitation <- Countries[Pop_GDPUSD_Sanitation][ ! region %in% "Aggregates"]

 GDPUSD_Electricity_CO2 <- Countries[GDPUSD_Electricity_CO2][ ! region %in% "Aggregates"]


 wPop_LifeExp_FertRt <- reshape(Pop_LifeExp_FertRt[, list(country, region, date, value, indicator)], v.names = "value", idvar=c("date", "country", "region"), timevar="indicator", direction = "wide")
 
 wPop_GDPUSD_HeadCnt <- reshape(Pop_GDPUSD_HeadCnt[, list(country, region, date, value, indicator)], v.names = "value", idvar=c("date", "country", "region"), timevar="indicator", direction = "wide")
 
 wPop_GDPUSD_Sanitation <- reshape(Pop_GDPUSD_Sanitation[, list(country, region, date, value, indicator)], v.names = "value", idvar=c("date", "country", "region"), timevar="indicator", direction = "wide")
 
 wGDPUSD_Electricity_CO2 <- reshape(GDPUSD_Electricity_CO2[, list(country, region, date, value, indicator)], v.names = "value", idvar=c("date", "country", "region"), timevar="indicator", direction = "wide")

 wPop_LifeExp_FertRt

 wGDPUSD_Electricity_CO2

 wPop_LifeExp_FertRt[, date := as.integer(date)]
 
 wPop_GDPUSD_HeadCnt[, date := as.integer(date)]
 
 wPop_GDPUSD_Sanitation[, date := as.integer(date)]
 
 wGDPUSD_Electricity_CO2[, date := as.integer(date)]
 

 setnames(wPop_LifeExp_FertRt, names(wPop_LifeExp_FertRt), c("Country", "Region", "Year", "Population", "Fertility", "LifeExpectancy"))

 setnames(wPop_GDPUSD_HeadCnt, names(wPop_GDPUSD_HeadCnt), c("Country", "Region", "Year", "Population", "GDPUSD", "PovertyHead"))

 setnames(wPop_GDPUSD_Sanitation, names(wPop_GDPUSD_Sanitation), c("Country", "Region", "Year", "Population", "GDPUSD", "SanitationAccess"))
 
 setnames(wGDPUSD_Electricity_CO2, names(wGDPUSD_Electricity_CO2), c("Country", "Region", "Year", "GDPUSD", "ElectricityConsumption", "CO2Emissions"))

 pltPop_LifeExp_FertRt <- gvisMotionChart(wPop_LifeExp_FertRt, idvar = "Country", timevar = "Year", xvar = "LifeExpectancy", yvar = "Fertility", sizevar = "Population", colorvar = "Region")
 
 plot(pltPop_LifeExp_FertRt)

 pltPop_GDPUSD_HeadCnt <- gvisMotionChart(wPop_GDPUSD_HeadCnt, idvar = "Country", timevar = "Year", xvar = "GDPUSD", yvar = "PovertyHead", sizevar = "Population", colorvar = "Region")
 
 plot(pltPop_GDPUSD_HeadCnt)
 
 pltPop_GDPUSD_Sanitation <- gvisMotionChart(wPop_GDPUSD_Sanitation, idvar = "Country", timevar = "Year", xvar = "GDPUSD", yvar = "SanitationAccess", sizevar = "Population", colorvar = "Region")
 
 plot(pltPop_GDPUSD_Sanitation)
 
 pltGDPUSD_Electricity_CO2 <- gvisMotionChart(wGDPUSD_Electricity_CO2, idvar = "Country", timevar = "Year", xvar = "GDPUSD", yvar = "ElectricityAccess", sizevar = "CO2Emissions", colorvar = "Region")
 
 plot(pltGDPUSD_Electricity_CO2)
