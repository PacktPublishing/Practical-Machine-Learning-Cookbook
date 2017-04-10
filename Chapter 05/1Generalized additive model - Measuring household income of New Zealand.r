
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

 devtools::install_github("ellisp/nzelect/pkg2")

 library(leaflet)
 library(nzcensus)
 library(Metrics)
 library(ggplot2)
 library(scales)
 library(boot)
 library(dplyr)
 library(Hmisc) 
 library(mgcv)  
 library(caret) 
 library(grid)
 library(stringr)
 library(ggrepel)
 library(glmnet)
 library(maps)

 tmp <- AreaUnits2013[AreaUnits2013$WGS84Longitude  0 & !is.na(AreaUnits2013$MedianIncome2013), ]

 palette <- colorQuantile("RdBu", NULL, n = 10)

 labels <- paste0(tmp$AU_NAM, " $", format(tmp$MedianIncome2013, big.mark = ","))

 leaflet() %>%
     addProviderTiles("CartoDB.Positron") %>%
     addCircles(lng = tmp$WGS84Longitude, lat = tmp$WGS84Latitude,
                color = pal(-tmp$MedianIncome2013),
                popup = labs,
                radius = 500) %>%
     addLegend(
         pal = pal,
         values = -tmp$MedianIncome2013,
         title = "Quantile of median<brhousehold income",
         position = "topleft",
         bins = 5)


 au <- AreaUnits2013 %>%
     select(-AU2014, -AU_NAM, -NZTM2000Easting, -NZTM2000Northing) %>%
     select(-PropWorked40_49hours2013, -Prop35to39_2013, -PropFemale2013)

 row.names(au) <- AreaUnits2013$AU_NAM


 names(au) <- gsub("_2013", "", names(au))

 names(au) <- gsub("2013", "", names(au))

 names(au) <- gsub("Prop", "", names(au))

 au <- au[complete.cases(au), ]

 data_use <- au

 dim(data_use)

 data_use <- data_use[the_data$WGS84Longitude > 100, ]

 names(data_use) <- make.names(names(data_use))
 
 names(data_use)
 
 reg_data <- spearman2(MedianIncome ~ ., data = data_use)
 reg_data[order(-reg_data[ ,6])[1:15], ]
 

 reg_formula <- terms(MedianIncome ~  
                         s(FullTimeEmployed, k = 6) + 
                         s(InternetHH, k = 6) +
                         s(NoQualification, k = 5) +
                         s(UnemploymentBenefit, k = 5) +
                         s(Smoker, k = 5) +
                         s(Partnered, k = 5)  +
                         s(Managers, k = 4) +
                         s(Bachelor, k = 4) +
                         s(SelfEmployed, k = 4) +
                         s(NoMotorVehicle, k = 4) +
                         s(Unemployed, k = 3) +
                         s(Labourers, k = 3) +
                         s(Worked50_59hours, k = 3) +
                         s(Separated, k = 3) +
                         s(Maori, k = 3) +
                         s(WGS84Longitude, WGS84Latitude) +
                         .,
                     data = data_use)



 gam_model <- gam(reg_formula, data = data_use)


 par(bty = "l", mar = c(5,4, 2, 1))
 
 
 par(mar = rep(2, 4))
 
 plot(gam_model, residuals = TRUE, pages = 1, shade = TRUE, seWithMean = TRUE, ylab = "")

 rmses_gam_boot <- boot(data = data_use, statistic = fit_gam, R = 99)

 rmses_gam_boot
 

 gam_rmse <- mean(rmses_gam_boot$t)
 
 gam_rmse
