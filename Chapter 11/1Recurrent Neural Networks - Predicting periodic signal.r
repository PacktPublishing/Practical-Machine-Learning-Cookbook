
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
 install.packages("rnn")
 library(rnn)

 set.seed(10)

 f <- 5
 
 w <- 2*pi*f
 
 t <- seq(0.005,2,by=0.005)

 x <- sin(t*w) + rnorm(200, 0, 0.25)

 y <- cos(t*w)

 X <- matrix(x, nrow = 40)

 
 Y <- matrix(y, nrow = 40)
 

 plot(as.vector(X), col='blue', type='l', ylab = "x-matrix, y-matrix", main = "Noisy waves")


 lines(as.vector(Y), col = "red")
 
 
 X <- (X - min(X)) / (max(X) - min(X))

 X
 
 Y <- (Y - min(Y)) / (max(Y) - min(Y))
 
 Y

 X <- t(X)

 Y <- t(Y)

 train <- 1:8

 test <- 9:10

 model <- trainr(Y = Y[train,],
                  X = X[train,],
                  learningrate = 0.05,
                  hidden_dim = 16,
                  numepochs = 1500)


 
 Y_predicted <- predictr(model, X)

 
 plot(as.vector(t(Y)), col = 'red', type = 'l', main = "Actual values vs Predicted values", ylab = "Y, Y-predicted")

 
 lines(as.vector(t(Y_predicted)), type = 'l', col = 'blue')
 
 
 plot(as.vector(t(Y[test,])), col = 'red', type='l', main = "Actual vs predicted: testing set", ylab = "Y,Y-predicted")
 

 
 lines(as.vector(t(Y_predicted[test,])), type = 'l', col = 'blue')
