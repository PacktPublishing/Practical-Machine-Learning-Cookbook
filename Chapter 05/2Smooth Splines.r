
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

 library(graphics)
 library(splines)
 
 matrx = matrix(cbind(1,.99, .99,1),nrow=2)
 
 cholsky = t(chol(matrx))

 nvars = dim(cholsky)[1]

 numobs = 1000

 
 set.seed(1)

 random_normal = matrix(rnorm(nvars*numobs,10,1), nrow=nvars, ncol=numobs)


 X = cholsky %*% random_normal


 
 newX = t(X)
 
 raw = as.data.frame(newX)

 head(raw)

 raw_original = as.data.frame(t(random_normal))
 
 names(raw) = c("response","predictor1")

 raw$predictor1_3 = raw$predictor1^3

 head(raw$predictor1_3)

 raw$predictor1_2 = raw$predictor1^2
 
 head(raw$predictor1_2)

 
 fit = lm(raw$response ~ raw$predictor1_3)
 
 
 fit

 plot(raw$response ~ raw$predictor1_3, pch=16, cex=.4, xlab="Predictor", ylab="Response", col ="red", main="Simulated Data with Slight Curve")
 
 
 abline(fit)

 x_axis <- with(cars, speed)
 
 y_axis <- with(cars, dist)


 eval_length = 50

 fit_loess <- loess.smooth(x_axis, y_axis, evaluation = eval_length, family="gaussian", span=.75, degree=1)
 
 fit_loess

 fit_loess_2 <- loess(y_axis ~ x_axis, family="gaussian", span=.75, degree=1)
 
 fit_loess_2

 new_x_axis = seq(min(x_axis),max(x_axis), length.out=eval_length)
 
 new_x_axis
 
 conf_int = cbind(
     predict(fit_loess_2, data.frame(x=new_x_axis)),
     predict(fit_loess_2, data.frame(x=new_x_axis))+
         predict(fit_loess_2, data.frame(x=new_x_axis), se=TRUE)$se.fit*qnorm(1-.05/2),
     predict(fit_loess_2, data.frame(x=new_x_axis))-
         predict(fit_loess_2, data.frame(x=new_x_axis), se=TRUE)$se.fit*qnorm(1-.05/2)
 )
 
 
 
 
 fit_lm = lm(y_axis ~ x_axis)
 
 
 fit_lm

 fit_poly = lm(y_axis ~ poly(x_axis,3) )
 
 fit_poly

 
 fit_nat_spline = lm(y_axis ~ ns(x_axis, 3) )
 
 fit_nat_spline

 fit_smth_spline = smooth.spline(y_axis ~ x_axis, nknots=15)
 
 fit_smth_spline

 
 plot(x_axis, y_axis, xlim=c(min(x_axis),max(x_axis)), ylim=c(min(y_axis),max(y_axis)), pch=16, cex=.5, ylab = "Stopping Distance (feet)", xlab= "Speed (MPH)", main="Comparison of Models", sub="Splines")
 

 matplot(new_x_axis, conf_int, lty = c(1,2,2), col=c(1,2,2), type = "l", add=T)

 
 lines(new_x_axis, predict(fit_lm, data.frame(x=new_x_axis)), col="red", lty=3)


 lines(new_x_axis, predict(fit_poly, data.frame(x=new_x_axis)), col="blue", lty=4)

 lines(new_x_axis, predict(fit_nat_spline, data.frame(x=new_x_axis)), col="green", lty=5)
 
 lines(fit_smth_spline, col="dark grey", lty=6)
 
 lines(ksmooth(x_axis, y_axis, "normal", bandwidth = 5), col = 'purple', lty=7)


