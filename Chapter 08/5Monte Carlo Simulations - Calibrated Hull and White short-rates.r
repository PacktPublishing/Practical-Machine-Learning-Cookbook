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
 install.packages("RQuantLib", type="binary")
 install.packages("ESGtoolkit")
 library(RQuantLib)
 library(ESGtoolkit)

 
 freq <- "monthly"

 delta_t <- 1/12
 
 delta_t

 params <- list(tradeDate=as.Date('2002-2-15'),
                 settleDate=as.Date('2002-2-19'),
                 payFixed=TRUE,
                 dt=delta_t,
                 strike=.06,
                 method="HWAnalytic",
                 interpWhat="zero",
                 interpHow= "spline")
 
 
 TermQuotes  <- list(d1w =0.0382,
                      d1m =0.0372,
                      d3m = 0.0363,
                      d6m = 0.0353,
                      d9m = 0.0348,
                      d1y = 0.0345,
                      s2y = 0.037125,
                      s3y =0.0398,
                      s5y =0.0443,
                      s10y =0.05165,
                      s15y =0.055175)
 
 
 SwaptionMaturities <- c(1,2,3,4,5)
 
 SwaptionMaturities
 
 SwapTenors <- c(1,2,3,4,5)
 
 SwapTenors
 
 VolatilityMatrix <- matrix(
      c(0.1490, 0.1340, 0.1228, 0.1189, 0.1148,
        0.1290, 0.1201, 0.1146, 0.1108, 0.1040,
        0.1149, 0.1112, 0.1070, 0.1010, 0.0957,
        0.1047, 0.1021, 0.0980, 0.0951, 0.1270,
        0.1000, 0.0950, 0.0900, 0.1230, 0.1160),
      ncol=5, byrow=TRUE)
 

 BermudanSwaption <- RQuantLib::BermudanSwaption(params, TermQuotes, SwaptionMaturities, SwapTenors, VolatilityMatrix)

 summary(BermudanSwaption)


 BermudanSwaption


 times <- seq(from = delta_t, to = 5, by = delta_t)


 DiscountCurve <- RQuantLib::DiscountCurve(params, TermQuotes, times)


 str(DiscountCurve)

 maturities <- DiscountCurve$times

 maturities

 
 MarketZeroRates <- DiscountCurve$zerorates
 
 MarketZeroRates
 
 MarketPrices <- DiscountCurve$discounts
 
 MarketPrices
 
 horizon <- 5
 
 NoSimulations <- 10000
 
 a <- BermudanSwaption$a
 a
 
 sigma <- BermudanSwaption$sigma
 
 sigma
 

 GaussianShocks <- ESGtoolkit::simshocks(n = NoSimulations, horizon = horizon, frequency = freq)

 x <- ESGtoolkit::simdiff(n = NoSimulations, horizon = horizon, frequency = freq, model = "OU", x0 = 0, theta1 = 0, theta2 = a, theta3 = sigma, eps = GaussianShocks)


 ForwardRates <- ts(replicate(nb.sims, DiscountCurve$forwards), start = start(x), deltat = deltat(x))

 t.out <- seq(from = 0, to = horizon, by = delta_t)

 param.alpha <- ts(replicate(NoSimulations, 0.5*(sigma^2)*(1 - exp(-a*t.out))^2/(a^2)), start = start(x), deltat = deltat(x))


 alpha <- ForwardRates + param.alpha


 ShortRates <- x + alpha

 StochasticDiscount <- ESGtoolkit::esgdiscountfactor(r = ShortRates, X = 1)
 
 MonteCarloPrices <- rowMeans(StochasticDiscount)
 
 MonteCarloPrices
 
 
 MonteCarloZeroRates <- -log(MonteCarloPrices)/maturities
 
 MonteCarloZeroRates
 
 ConfidenceInterval <- t(apply((StochasticDiscount - MarketPrices)[-1, ], 1, function(x) t.test(x)$conf.int))


 head(ConfidenceInterval)
 
 
 par(mfrow = c(2, 2))
 
 
 
 ESGtoolkit::esgplotbands(ShortRates, xlab = "maturities", ylab = "short-rate quantiles", main = "Short Rate Quantiles") 
 
 
 plot(maturities, MonteCarloZeroRates, type='l', col = 'blue', lwd = 1, main = "Monte Carlo v/s Market n Zero Rates")

 points(maturities, MonteCarloZeroRates, col = 'red')


 plot(maturities, MonteCarloPrices, type='l', col = 'blue', lwd = 1, main = "Monte Carlo v/s Market n Zero Rates")
 

 
 points(maturities, MonteCarloPrices, col = 'red')

 
 matplot(maturities[-1], conf.int, type = 'l', main = "Confidence Interval for the price difference")


 

