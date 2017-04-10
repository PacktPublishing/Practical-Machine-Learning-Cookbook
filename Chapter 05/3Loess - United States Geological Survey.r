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

 library(dataRetrieval)
 library(dplyr)

 siteNumber<-c("01538000")

 parameterCd <- "00060"
 
 
 Q_daily <- readNWISdv(siteNumber, parameterCd)

 
 tail(Q_daily)

 
 str(Q_daily)


 Q_daily <- renameNWISColumns(Q_daily)
 
 tail(Q_daily)
 
 stationInfo <- readNWISsite(siteNumber)

 if(as.numeric(diff(range(Q_daily$Date))) != (nrow(Q_daily)+1)){
     fullDates <- seq(from=min(Q_daily$Date),
                      to = max(Q_daily$Date), by="1 day")
     fullDates <- data.frame(Date = fullDates, 
                             agency_cd = Q_daily$agency_cd[1],
                             site_no = Q_daily$site_no[1],
                             stringsAsFactors = FALSE)
     Q_daily <- full_join(Q_daily, fullDates,
                         by=c("Date","agency_cd","site_no")) %>%
         arrange(Date)
 }
 
 moving_avg <- function(x,n=30){stats::filter(x,rep(1/n,n), sides=1)}


 Q_daily <- Q_daily %>% mutate(rollMean = as.numeric(moving_avg(Flow)), day.of.year = as.numeric(strftime(Date, format = "%j")))

 tail(Q_daily)

 
 Q_summary <- Q_daily %>% 
     group_by(day.of.year) %>%
     summarize(p75 = quantile(rollMean, probs = .75, na.rm = TRUE),
               p25 = quantile(rollMean, probs = .25, na.rm = TRUE),
               p10 = quantile(rollMean, probs = 0.1, na.rm = TRUE),
               p05 = quantile(rollMean, probs = 0.05, na.rm = TRUE),
               p00 = quantile(rollMean, probs = 0, na.rm = TRUE)) 
 
 current_year <- as.numeric(strftime(Sys.Date(), format = "%Y"))

 
 summary.0 <- Q_summary %>% mutate(Date = as.Date(day.of.year - 1, origin = paste0(current_year-2,"-01-01")), day.of.year = day.of.year - 365)


 summary.1 <- Q_summary %>% mutate(Date = as.Date(day.of.year - 1, origin = paste0(current_year-1,"-01-01")))


 summary.2 <- Q_summary %>% mutate(Date = as.Date(day.of.year - 1, origin = paste0(current_year,"-01-01")), day.of.year = day.of.year  365)
 

 Q_summary <- bind_rows(summary.0, summary.1, summary.2) 
 
 Q_summary

 smooth.span <- 0.3


 Q_summary$sm.75 <- predict(loess(p75~day.of.year, data = Q_summary, span = smooth.span))


 head(Q_summary$sm.75)

 Q_summary$sm.25 <- predict(loess(p25~day.of.year, data = Q_summary, span = smooth.span))
 
 head(summaryQ$sm.25)

 Q_summary$sm.10 <- predict(loess(p10~day.of.year, data = Q_summary, span = smooth.span))
 
 head(summaryQ$sm.10)
 
 Q_summary$sm.05 <- predict(loess(p05~day.of.year, data = Q_summary, span = smooth.span))
 
 head(summaryQ$sm.05)

 Q_summary$sm.00 <- predict(loess(p00~day.of.year, data = Q_summary, span = smooth.span))
 
 head(summaryQ$sm.00)

 Q_summary <- select(Q_summary, Date, day.of.year, sm.75, sm.25, sm.10, sm.05, sm.00) %>% filter(Date = as.Date(paste0(current_year-1,"-01-01")))
 
 Q_summary

 latest.years <- Q_daily %>% filter(Date = as.Date(paste0(current_year-1,"-01-01"))) %>% mutate(day.of.year = 1:nrow(.))

 title.text <- paste0(stationInfo$station_nm,"\n", "Provisional Data - Subject to change\n", "Record Start = ", min(Q_daily$Date), "  Number of years = ", as.integer(as.numeric(difftime(time1 = max(Q_daily$Date), time2 = min(Q_daily$Date), units = "weeks"))/52.25), "\nDate of plot = ",Sys.Date(), "  Drainage Area = ",stationInfo$drain_area_va, "mi^2")

 
 mid.month.days <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
 
 month.letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")
 
 start.month.days <- c(1, 32, 61, 92, 121, 152, 182, 214, 245, 274, 305, 335)
 
 label.text <- c("Normal","Drought Watch","Drought Warning","Drought Emergency")
 
 
 year1_summary <- data.frame(Q_summary[2:366,])

 head(year1_summary)

 year2_summary <- data.frame(Q_summary[367:733,])
 
 head(year2_summary)

 simple.plot <- ggplot(data = Q_summary, aes(x = day.of.year)) 
     geom_ribbon(aes(ymin = sm.25, ymax = sm.75, fill = "Normal")) 
     geom_ribbon(aes(ymin = sm.10, ymax = sm.25, fill = "Drought Watch")) 
     geom_ribbon(aes(ymin = sm.05, ymax = sm.10, fill = "Drought Warning")) 
     geom_ribbon(aes(ymin = sm.00, ymax = sm.05, fill = "Drought Emergency")) 
     scale_y_log10(limits = c(1,1000)) 
     geom_line(data = latest.years, aes(x=day.of.year, y=rollMean, color = "30-Day Mean"),size=2) 
     geom_vline(xintercept = 365) 
 
 simple.plot


