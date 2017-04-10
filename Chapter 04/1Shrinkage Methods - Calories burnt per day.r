R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
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
 install.packages("glmnet")
 install.packages("dplyr")
 install.packages("tidyr")
 install.packages("ggplot2")
 install.packages("caret")
 install.packages("glmnet")
 install.packages("boot")
 install.packages("RColorBrewer")
 install.packages("glmnet")
 install.packages("Metrics")

 library(dplyr)
 library(tidyr)
 library(ggplot2)
 library(caret)
 library(glmnet)
 library(boot)
 library(RColorBrewer)
 library(glmnet)
 library(Metrics)

 fitbit_details <- read.csv("https://raw.githubusercontent.com/ellisp/ellisp.github.io/source/data/fitbit_export_20160806.csv", 
                    skip = 1, stringsAsFactors = FALSE) %>%
     mutate(
         Calories.Burned = as.numeric(gsub(",", "", Calories.Burned)),
         Steps = as.numeric(gsub(",", "", Steps)),
         Activity.Calories = as.numeric(gsub(",", "", Activity.Calories)),
         Date = as.Date(Date, format = "%d/%m/%Y")
     )
 
 fitbit <- fitbit_details

 head(fitbit)


 
 fitbit$Activity.Calories <- NULL
 
 fitbit$Date <- NULL

 fitbit$Steps <- fitbit$Steps / 1000

 fitbit$Steps
 
 panel_correlations <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
 usr <- par("usr"); on.exit(par(usr))
 par(usr = c(0, 1, 0, 1))
 r <- abs(cor(x, y))
 txt <- format(c(r, 0.123456789), digits = digits)[1]
 txt <- paste0(prefix, txt)
 if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
 text(0.5, 0.5, txt, cex = cex.cor * r)
 }
 
 pairs(fitbit[ , -1], lower.panel = panel_correlations, main = "Pairwise Relationship - Fitbit's Measured Activities")

 ggplot(fitbit, aes(x = Distance / Steps)) + geom_rug() + geom_density() +ggtitle("Stride Length Reverse- Engineered from Fitbit Data", subtitle = "Not all strides identical, due to rounding or other jitter")
 
 moderate <- lm(Calories.Burned ~ Steps, data = fitbit)
 
 moderate


 round(coef(moderate))
 
 plot(moderate, which = 1, bty = "l", main ="Predicted Calories compared with Residuals")

 pacf(resid(moderate), main = "Partial Autocorrelation of residuals from single variable regression")
 grid()

 X <- as.matrix(fitbit[ , -1])

 head(X)
 
 Y <- fitbit$Calories.Burned
 
 Y
 

 set.seed(123)


 alphas <- seq(from = 0, to  = 1, length.out = 10)


 res <- matrix(0, nrow = length(alphas), ncol = 6)


 for(i in 1:length(alphas)){
     for(j in 2:6){
         cvmod <- cv.glmnet(X, Y, alpha = alphas[i])
         res[i, c(1, j)] <- c(alphas[i], sqrt(min(cvmod$cvm)))
     }
 }
 
 res <- data.frame(res)
 
 res
 
 res$average_rmse <- apply(res[ , 2:6], 1, mean)
 
 res$average_rmse
 [1] 109.2894 108.6945 108.4060 109.9822 111.0122 108.6028 106.0425 108.7103 109.5337 109.7035
 
 
 
 res <- res[order(res$average_rmse), ]
 
 res
 
 
 names(res)[1] <- "alpha"

 res %>%
     select(-average_rmse) %>%
     gather(trial, rmse, -alpha) %>%
     ggplot(aes(x = alpha, y = rmse)) +
     geom_point() +
     geom_smooth(se = FALSE) +
     labs(y = "Root Mean Square Error") +
     ggtitle("Cross Validation best RMSE for differing values of alpha")


 
 bestalpha <- res[1, 1]
 
 bestalpha
 
 crossvalidated <- cv.glmnet(X, Y, alpha = bestalpha)
 
 
 moderate1 <- glmnet(X, Y, alpha = bestalpha)
 

 OLSmodel <- lm(Calories.Burned ~ ., data = fitbit)
 
 OLSmodel

 
 coeffs <- data.frame(original = coef(OLSmodel), 
                     shrunk =  as.vector(coef(moderate1, s = crossvalidated$lambda.min)),
                     very.shrunk = as.vector(coef(moderate1, s = crossvalidated$lambda.1se)))
 
 coeffs

 
 round(coeffs, 3)

 moderate2 <- glmnet(X, Y, lambda = 0)


 moderate2

 
 round(data.frame("elastic, lambda = 0" = as.vector(coef(moderate2)), "lm" = coef(OLSmodel), check.names = FALSE), 3)

 
 moderate3 <- glmnet(X[ , -2], Y, lambda = 0)
 
 moderate3

 
 moderate4 <- lm(Y ~ X[ , -2])
 
 moderate4
 
 
 round(data.frame("elastic, lambda = 0" = as.vector(coef(moderate3)), "lm" = coef(moderate4), check.names = FALSE), 3)

 
 > modellingfucn1 <- function(data, i){
     X <- as.matrix(data[i , -1])
     Y <- data[i , 1]
# k-fold cross-validation for glmnet
     crossvalidated <- cv.glmnet(X, Y, alpha = 1, nfolds = 30)
# Fitting a generalized linear model via penalized maximum likelihood
     moderate1 <- glmnet(X, Y, alpha = 1)
# Computing the root mean squared error
     rmse(predict(moderate1, newx = as.matrix(data[ , -1]), s = crossvalidated$lambda.min), data[ , 1])
 }

 

 elastic_boot <- boot(fitbit, statistic = modellingfucn1, R = 99)


 elastic_boot

 
 
 modellingfucn2 <- function(data, i){
     OLSmodel <- lm(Calories.Burned ~ ., data = data[i, ])
     rmse(predict(OLSmodel, newdata = data), data[ , 1])
 }

 lm_boot <- boot(fitbit, statistic = modellingfucn2, R = 99)
 
 lm_boot

 
 
 modellingOLS <- function(data, i){
     mod0 <- lm(Calories.Burned ~ Steps, data = data[i, ])
     rmse(predict(moderate, newdata = data), data[ , 1])
 }
 
 
 lmOLS_boot <- boot(fitbit, statistic = modellingOLS, R = 99)
 
 lmOLS_boot


 
 round(c("elastic modelling" = mean(elastic_boot$t), 
         "OLS modelling" = mean(lm_boot$t),
         "OLS modelling, only one explanatory variable" = mean(lmOLS_boot$t)), 1)
                           elastic modelling                                OLS modelling 
                                        95.8                                         99.7 
OLS modelling, only one explanatory variable 
                                       159.7 
 
 
 
 
 ordering <- c(7,5,6,2,1,3,4)
 par(mar = c(5.1, 4.1, 6.5, 1), bg = "grey90")

 model_scaled <- glmnet(scale(X), Y, alpha = bestalpha)
 
 the_palette <- brewer.pal(7, "Set1")
 
 
 plot(model_scaled, xvar = "dev", label = TRUE, col = the_palette, lwd = 2, main = "Increasing contribution of different explanatory variables\nas penalty for including them is relaxed")

 legend("topleft", legend = colnames(X)[ordering], text.col = the_palette[ordering], lwd = 2, bty = "n", col = the_palette[ordering])

