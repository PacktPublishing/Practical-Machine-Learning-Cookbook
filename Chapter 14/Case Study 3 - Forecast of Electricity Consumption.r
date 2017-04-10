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

 install.packages("feather")
 install.packages("data.table")
 install.packages("ggplot2")
 install.packages("plotly")
 install.packages("animation")

 library(feather)
 library(data.table)
 library(ggplot2)
 library(plotly)
 library(animation)

 AggData <- as.data.table(read_feather("d:/DT_4_ind"))


 str(AggData)

 

 head(AggData)

 
 
 ggplot(data = AggData, aes(x = date, y = value)) +
     geom_line() + 
     facet_grid(type ~ ., scales = "free_y") +
     theme(panel.border = element_blank(),
           panel.background = element_blank(),
           panel.grid.minor = element_line(colour = "grey90"),
           panel.grid.major = element_line(colour = "green"),
           panel.grid.major.x = element_line(colour = "red"),
           axis.text = element_text(size = 10),
           axis.title = element_text(size = 12, face = "bold"),
           strip.text = element_text(size = 9, face = "bold")) +
     labs(title = "Electricity Consumption - Industry", x = "Date", y = "Load (kW)")
 
 

 AggData
 

 AggData[, week_num := as.integer(as.factor(AggData[, week]))]


 AggData
 
 
 n_type <- unique(AggData[, type])

 n_type


 n_date <- unique(AggData[, date])
 
 
 n_weekdays <- unique(AggData[, week])

 period <- 48

 data_reg <- AggData[(type == n_type[2] & date %in% n_date[57:70])]

 data_reg


 ggplot(data_reg, aes(date_time, value)) +
     geom_line() +
     theme(panel.border = element_blank(),
           panel.background = element_blank(),
           panel.grid.minor = element_line(colour = "grey90"),
           panel.grid.major = element_line(colour = "green"),
           panel.grid.major.x = element_line(colour = "red"),
           axis.text = element_text(size = 10),
           axis.title = element_text(size = 12, face = "bold")) +
     labs(title = "Regression Analysis - Education Buildings", x = "Date", y = "Load (kW)")
 

 N <- nrow(data_reg)

 trainset_window <- N / period

 matrix_train <- data.table(Load = data_reg[, value], Daily = as.factor(rep(1:period, trainset_window)), Weekly = as.factor(data_reg[, week_num]))
 
 matrix_train

 linear_model_1 <- lm(Load ~ 0 + ., data = matrix_train)

 linear_model_1
 
 summary_1 <- summary(linear_model_1)
 
 summary_1

 paste("R-squared: ", round(summary_1$r.squared, 3), ", p-value of F test: ", 1-pf(summary_1$fstatistic[1], summary_1$fstatistic[2], summary_1$fstatistic[3]))

 
 
 
 datas <- rbindlist(list(data_reg[, .(value, date_time)], data.table(value = linear_model_1$fitted.values, data_time = data_reg[, date_time])))
 
 datas

 
 datas[, type := rep(c("Real", "Fitted"), each = nrow(data_reg))]
 
 datas

 
 ggplot(data = datas, aes(date_time, value, group = type, colour = type)) + geom_line(size = 0.8) + theme_bw() +
     labs(x = "Time", y = "Load (kW)", title = "Fit from Multiple Linear Regression")
 
 ggplot(data = data.table(Fitted_values = linear_model_1$fitted.values,
                          Residuals = linear_model_1$residuals),
        aes(Fitted_values, Residuals)) +
     geom_point(size = 1.7) +
     geom_smooth() +
     geom_hline(yintercept = 0, color = "red", size = 1) +
     labs(title = "Fitted values vs Residuals")


 ggQQ <- function(lm){
     d <- data.frame(std.resid = rstandard(lm))
     y <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
     x <- qnorm(c(0.25, 0.75))
     slope <- diff(y)/diff(x)
     int <- y[1L] - slope * x[1L]
     
     p <- ggplot(data = d, aes(sample = std.resid)) +
         stat_qq(shape = 1, size = 3) +         
         labs(title = "Normal Q-Q",             
              x = "Theoretical Quantiles",      
              y = "Standardized Residuals") +   
         geom_abline(slope = slope, intercept = int, linetype = "dashed",
                     size = 1, col = "firebrick1") 
     return(p)
 }


 ggQQ(linear_model_1)

 linear_model_2 <- lm(Load ~ 0 + Daily + Weekly + Daily:Weekly, data = matrix_train)
 
 linear_model_2

 
 c(Previous = summary(linear_model_1)$r.squared, New = summary(linear_model_2)$r.squared)

 
 ggplot(data.table(Residuals = c(linear_model_1$residuals, linear_model_2$residuals), Type = c(rep("Multiple Linear Reg - simple", nrow(data_reg)), rep("Multiple Linear Reg with interactions", nrow(data_reg)))), aes(Type, Residuals, fill = Type)) + geom_boxplot()


 ggplotly()

 datas <- rbindlist(list(data_reg[, .(value, date_time)], data.table(value = linear_model_2$fitted.values, data_time = data_reg[, date_time])))
 
 datas

  
 datas[, type := rep(c("Real", "Fitted"), each = nrow(data_reg))]
 
 datas
 
 ggplot(data = datas, aes(date_time, value, group = type, colour = type)) + geom_line(size = 0.8) + theme_bw() + labs(x = "Time", y = "Load (kW)", title = "Fit from Multiple Linear Reg")


 ggplot(data = data.table(Fitted_values = linear_model_2$fitted.values, Residuals = linear_model_2$residuals), aes(Fitted_values, Residuals)) + geom_point(size = 1.7) + geom_hline(yintercept = 0, color = "red", size = 1) +
 labs(title = "Fitted values vs Residuals")

 ggQQ(linear_model_2)
 
 
 predWeekReg <- function(data, set_of_date){
     data_train <- data[date %in% set_of_date]
     
     N <- nrow(data_train)
     window <- N / period # number of days in the train set
     matrix_train <- data.table(Load = data_train[, value],
                                Daily = as.factor(rep(1:period, window)),
                                Weekly = as.factor(data_train[, week_num]))
     lm_m <- lm(Load ~ 0 + Daily + Weekly + Daily:Weekly, data = matrix_train)
     
     pred_week <- predict(lm_m, matrix_train[1:(7*period), -1, with = FALSE])
     
     return(as.vector(pred_week))
 }


 mape <- function(real, pred){
     return(100 * mean(abs((real - pred)/real)))
 }
 


 n_weeks <- floor(length(n_date)/7) - 2
 
 n_weeks



 lm_pred_weeks_1 <- sapply(0:(n_weeks-1), function(i)
     predWeekReg(AggData[type == n_type[1]], n_date[((i*7)+1):((i*7)+7*2)]))
 

 lm_pred_weeks_2 <- sapply(0:(n_weeks-1), function(i)
     predWeekReg(AggData[type == n_type[2]], n_date[((i*7)+1):((i*7)+7*2)]))
 
 
 lm_pred_weeks_3 <- sapply(0:(n_weeks-1), function(i)
     predWeekReg(AggData[type == n_type[3]], n_date[((i*7)+1):((i*7)+7*2)]))
 

 lm_pred_weeks_4 <- sapply(0:(n_weeks-1), function(i)
     predWeekReg(AggData[type == n_type[4]], n_date[((i*7)+1):((i*7)+7*2)]))
 

 lm_err_mape_1 <- sapply(0:(n_weeks-1), function(i)
     mape(AggData[(type == n_type[1] & date %in% n_date[(15+(i*7)):(21+(i*7))]), value],
          lm_pred_weeks_1[, i+1]))

 
 lm_err_mape_1

 lm_err_mape_2 <- sapply(0:(n_weeks-1), function(i)
     mape(AggData[(type == n_type[2] & date %in% n_date[(15+(i*7)):(21+(i*7))]), value],
          lm_pred_weeks_2[, i+1]))
 
 lm_err_mape_2
 
 
 lm_err_mape_3 <- sapply(0:(n_weeks-1), function(i)
     mape(AggData[(type == n_type[3] & date %in% n_date[(15+(i*7)):(21+(i*7))]), value],
          lm_pred_weeks_3[, i+1]))
 
 lm_err_mape_3
 
 lm_err_mape_4 <- sapply(0:(n_weeks-1), function(i)
     mape(AggData[(type == n_type[4] & date %in% n_date[(15+(i*7)):(21+(i*7))]), value],
          lm_pred_weeks_4[, i+1]))

 lm_err_mape_4


 datas <- data.table(value = c(as.vector(lm_pred_weeks_1),
                               AggData[(type == n_type[1]) & (date %in% n_date[-c(1:14,365)]), value]),
                     date_time = c(rep(AggData[-c(1:(14*48), (17473:nrow(AggData))), date_time], 2)),
                     type = c(rep("MLR", nrow(lm_pred_weeks_1)*ncol(lm_pred_weeks_1)),
                              rep("Real", nrow(lm_pred_weeks_1)*ncol(lm_pred_weeks_1))),
                     week = c(rep(1:50, each = 336), rep(1:50, each = 336)))


 

 saveGIF({
  oopt = ani.options(interval = 0.9, nmax = 50)
  for(i in 1:ani.options("nmax")){
    print(ggplot(data = datas[week == i], aes(date_time, value, group = type, colour = type)) +
            geom_line(size = 0.8) +
            scale_y_continuous(limits = c(min(datas[, value]), max(datas[, value]))) + 
            theme(panel.border = element_blank(), panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey90"),
                  panel.grid.major = element_line(colour = "grey90"),
                  panel.grid.major.x = element_line(colour = "grey90"),
                  title = element_text(size = 15),
                  axis.text = element_text(size = 10),
                  axis.title = element_text(size = 12, face = "bold")) +
            labs(x = "Time", y = "Load (kW)",
                 title = paste("Forecast of MLR (", n_type[1], "); ", "week: ", i, "; MAPE: ",
                               round(lm_err_mape_1[i], 2), "%", sep = "")))
    ani.pause()
  }}, movie.name = "industry_1.gif", ani.height = 450, ani.width = 750)