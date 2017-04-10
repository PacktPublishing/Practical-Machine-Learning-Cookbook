gala <- read.table("d:/gala.txt")
regpois <- glm( Species ~ Area + Elevation + Nearest, family=poisson, data=gala)
summary(regpois)

Call:
glm(formula = Species ~ Area + Elevation + Nearest, family = poisson, 
    data = gala)

Deviance Residuals: 
     Min        1Q    Median        3Q       Max  
-17.1900   -6.1715   -2.7125    0.7063   21.4237  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  3.548e+00  3.933e-02  90.211  < 2e-16 ***
Area        -5.529e-05  1.890e-05  -2.925  0.00344 ** 
Elevation    1.588e-03  5.040e-05  31.502  < 2e-16 ***
Nearest      5.921e-03  1.466e-03   4.039 5.38e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 3510.7  on 29  degrees of freedom
Residual deviance: 1797.8  on 26  degrees of freedom
AIC: 1966.7

Number of Fisher Scoring iterations: 5


plot(regpois$fit,gala$Species)
p <- ppois(gala$Species,regpois$fit)
hist(p,breaks=10)
ks.test(p,"punif")

	One-sample Kolmogorov-Smirnov test

data:  p
D = 0.57731, p-value = 4.134e-09
alternative hypothesis: two-sided


p <- 0.5*(ppois(gala$Species,regpois$fit) + ppois(gala$Species-1,regpois$fit))
hist(p,breaks=10)
ks.test(p,"punif")

	One-sample Kolmogorov-Smirnov test

data:  p
D = 0.58571, p-value = 2.3e-09
alternative hypothesis: two-sided

regpois2 <- glm( Species ~ Area + Elevation + Nearest, family=poisson(link=sqrt), data=gala)
summary(regpois2)

Call:
glm(formula = Species ~ Area + Elevation + Nearest, family = poisson(link = sqrt), 
    data = gala)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-19.108   -5.129   -1.335    1.846   16.918  

Coefficients:
              Estimate Std. Error z value Pr(>|z|)    
(Intercept)  4.1764222  0.1446592  28.871  < 2e-16 ***
Area        -0.0004844  0.0001655  -2.926  0.00343 ** 
Elevation    0.0110143  0.0003372  32.664  < 2e-16 ***
Nearest      0.0083908  0.0065858   1.274  0.20264    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 3510.7  on 29  degrees of freedom
Residual deviance: 1377.5  on 26  degrees of freedom
AIC: 1546.3

Number of Fisher Scoring iterations: 5


p2 <- 0.5*(ppois(gala$Species,regpois2$fit) + ppois(gala$Species-1,regpois2$fit))
hist(p2,breaks=10)  
ks.test(p2,"punif")

	One-sample Kolmogorov-Smirnov test

data:  p2
D = 0.47262, p-value = 3.023e-06
alternative hypothesis: two-sided

reg <- lm(Species ~ Area+Elevation+Nearest, data=gala)
summary(reg)

Call:
lm(formula = Species ~ Area + Elevation + Nearest, data = gala)

Residuals:
     Min       1Q   Median       3Q      Max 
-191.856  -33.111  -18.626    5.673  262.209 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept) 16.46471   23.38884   0.704  0.48772   
Area         0.01908    0.02676   0.713  0.48216   
Elevation    0.17134    0.05452   3.143  0.00415 **
Nearest      0.07123    1.06481   0.067  0.94718   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 80.84 on 26 degrees of freedom
Multiple R-squared:  0.5541,	Adjusted R-squared:  0.5027 
F-statistic: 10.77 on 3 and 26 DF,  p-value: 8.817e-05

plot(reg)


reg2 <- lm(sqrt(Species) ~ Area+Elevation+Nearest, data=gala)
summary(reg2)

Call:
lm(formula = sqrt(Species) ~ Area + Elevation + Nearest, data = gala)

Residuals:
    Min      1Q  Median      3Q     Max 
-8.8057 -2.1775 -0.2086  1.3943  8.8730 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  3.744e+00  1.072e+00   3.492 0.001729 ** 
Area        -2.253e-05  1.227e-03  -0.018 0.985485    
Elevation    9.795e-03  2.499e-03   3.920 0.000576 ***
Nearest      2.002e-02  4.880e-02   0.410 0.685062    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.705 on 26 degrees of freedom
Multiple R-squared:  0.5799,	Adjusted R-squared:  0.5315 
F-statistic: 11.96 on 3 and 26 DF,  p-value: 4.144e-05

plot(reg2)
shapiro.test(reg2$res)

	Shapiro-Wilk normality test

data:  reg2$res
W = 0.9633, p-value = 0.375

reg3 <- lm(log(Species) ~ Area+Elevation+Nearest, data=gala)
summary(reg3)

Call:
lm(formula = log(Species) ~ Area + Elevation + Nearest, data = gala)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.0739 -0.5161  0.3307  0.7472  1.6271 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.3724325  0.3448586   6.879 2.65e-07 ***
Area        -0.0002687  0.0003946  -0.681  0.50197    
Elevation    0.0029096  0.0008039   3.620  0.00125 ** 
Nearest      0.0133869  0.0157001   0.853  0.40163    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.192 on 26 degrees of freedom
Multiple R-squared:  0.4789,	Adjusted R-squared:  0.4187 
F-statistic: 7.964 on 3 and 26 DF,  p-value: 0.0006281

plot(reg3)
shapiro.test(reg3$res)

	Shapiro-Wilk normality test

data:  reg3$res
W = 0.91925, p-value = 0.02565

