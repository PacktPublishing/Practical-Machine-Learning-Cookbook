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

[Workspace loaded from ~/.RData
 install.packages("depmixS4")
 install.packages("quantmod")
 install.packages("ggplot2")
 library(depmixS4)
 library(quantmod)
 library(ggplot2)
 set.seed(1)

 getSymbols( "^GSPC", from="2004-01-01" )

 GSPCDiff = diff( log( Cl( GSPC ) ) )

 str(GSPCDiff)
An ‘xts’ object on 2004-01-02/2016-11-30 containing:
  Data: num [1:3252, 1] NA 0.01232 0.00129 0.00236 0.00495 ...
 - attr(*, "dimnames")=List of 2
  ..$ : NULL
  ..$ : chr "GSPC.Close"
  Indexed by objects of class: [Date] TZ: UTC
  xts Attributes:  
List of 2
 $ src    : chr "yahoo"
 $ updated: POSIXct[1:1], format: "2016-12-01 23:38:20"


 
 head(GSPCDiff)
             GSPC.Close
2004-01-02           NA
2004-01-05  0.012319151
2004-01-06  0.001291313
2004-01-07  0.002364367
2004-01-08  0.004950824
2004-01-09 -0.008927336
 
 returns = as.numeric(GSPCDiff)
 
 plot(GSPCDiff)

 hmm2states <- depmix(returns ~ 1, family = gaussian(), nstates = 2, data=data.frame(returns=returns))

 hmm2states
Initial state probabilties model 
pr1 pr2 
0.5 0.5 

Transition matrix 
       toS1 toS2
fromS1  0.5  0.5
fromS2  0.5  0.5

Response parameters 
Resp 1 : gaussian 
    Re1.(Intercept) Re1.sd
St1               0      1
St2               0      1
 
 hmmfit2states <- fit(hmm2states, verbose = FALSE)
converged at iteration 37 with logLik: 10518.77 

 hmmfit2states
Convergence info: Log likelihood converged to within tol. (relative change) 
'log Lik.' 10518.77 (df=7)
AIC:  -21023.55 
BIC:  -20980.95

 
 PosteriorProbs <- posterior(hmmfit2states)


 head(PosteriorProbs)
  state          S1        S2
1     2 0.000000000 1.0000000
2     2 0.007586430 0.9924136
3     2 0.002517719 0.9974823
4     2 0.002560062 0.9974399
5     2 0.002888478 0.9971115
6     2 0.005725764 0.9942742
 

 
 plot(returns, type='l', main='Regime Detection', xlab='No of Observations', ylab='Returns')


 matplot(PosteriorProbs[,-1], type='l', main='Regime Posterior Probabilities', xlab='No of Observations', ylab='Probability')

 hmm3states <- depmix(returns ~ 1, family = gaussian(), nstates = 3, data=data.frame(returns=returns))

 hmm3states
Initial state probabilties model 
  pr1   pr2   pr3 
0.333 0.333 0.333 

Transition matrix 
        toS1  toS2  toS3
fromS1 0.333 0.333 0.333
fromS2 0.333 0.333 0.333
fromS3 0.333 0.333 0.333

Response parameters 
Resp 1 : gaussian 
    Re1.(Intercept) Re1.sd
St1               0      1
St2               0      1
St3               0      1
 

 hmmfit3states <- fit(hmm3states, verbose = FALSE)
converged at iteration 102 with logLik: 10659.7 
 

 
 PosteriorProbs <- posterior(hmmfit3states)
 
 head(PosteriorProbs)
  state        S1         S2          S3
1     1 1.0000000 0.00000000 0.000000000
2     1 0.9780159 0.01904906 0.002935031
3     2 0.9196440 0.07866273 0.001693291
4     2 0.8488129 0.14960389 0.001583215
5     2 0.7605025 0.23798856 0.001508972
6     2 0.8433328 0.15467220 0.001995043
 
 layout(1:2)
 
 plot(returns, type='l', main='Regime Detection', xlab='No of Observations', ylab='Returns')
 
 matplot(PosteriorProbs[,-1], type='l', main='Regime Posterior Probabilities', xlab='No of Observations', ylab='Probability')

 hmm4states <- depmix(returns ~ 1, family = gaussian(), nstates = 4, data=data.frame(returns=returns))

 hmm4states
Initial state probabilties model 
 pr1  pr2  pr3  pr4 
0.25 0.25 0.25 0.25 

Transition matrix 
       toS1 toS2 toS3 toS4
fromS1 0.25 0.25 0.25 0.25
fromS2 0.25 0.25 0.25 0.25
fromS3 0.25 0.25 0.25 0.25
fromS4 0.25 0.25 0.25 0.25

Response parameters 
Resp 1 : gaussian 
    Re1.(Intercept) Re1.sd
St1               0      1
St2               0      1
St3               0      1
St4               0      1
 
 hmmfit4states <- fit(hmm4states, verbose = FALSE)
converged at iteration 426 with logLik: 10684.96 

 PosteriorProbs <- posterior(hmmfit4states)

 head(PosteriorProbs)

 layout(1:2)

 plot(returns, type='l', main='Regime Detection', xlab='No of Observations', ylab='Returns')

 matplot(PosteriorProbs[,-1], type='l', main='Regime Posterior Probabilities', xlab='No of Observations', ylab='Probability')
