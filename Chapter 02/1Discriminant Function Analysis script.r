
R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
Copyright (C) 2015 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
library(MASS)

brine <- read.table("d:/brine.txt", header=TRUE, sep=",", row.names=1)
head(brine)

  HCO3  SO4     Cl    Ca    Mg     Na GROUP
1 10.4 30.0  967.1  95.9  53.7  857.7     1
2  6.2 29.6 1174.9 111.7  43.9 1054.7     1
3  2.1 11.4 2387.1 348.3 119.3 1932.4     1
4  8.5 22.5 2186.1 339.6  73.6 1803.4     1
5  6.7 32.8 2015.5 287.6  75.1 1691.8     1
6  3.8 18.9 2175.8 340.4  63.8 1793.9     1

pairs(brine[ ,1:6])

brine.log <- brine
brine.log[ ,1:6] <- log(brine[ ,1:6]+1)
pairs(brine.log[ ,1:6])


brine.log.lda <- lda(GROUP ~ HCO3 + SO4 + Cl + Ca + Mg + Na, data=brine.log)

brine.log.lda

Call:
lda(GROUP ~ HCO3 + SO4 + Cl + Ca + Mg + Na, data = brine.log)

Prior probabilities of groups:
        1         2         3 
0.3684211 0.3157895 0.3157895 

Group means:
      HCO3      SO4       Cl       Ca       Mg       Na
1 1.759502 3.129009 7.496891 5.500942 4.283490 7.320686
2 2.736481 3.815399 6.829565 4.302573 4.007725 6.765017
3 1.374438 2.378965 6.510211 4.641049 3.923851 6.289692

Coefficients of linear discriminants:
              LD1         LD2
HCO3  -1.67799521  0.64415802
SO4    0.07983656  0.02903096
Cl    22.27520614 -0.31427770
Ca    -1.26859368  2.54458682
Mg    -1.88732009 -2.89413332
Na   -20.86566883  1.29368129

Proportion of trace:
   LD1    LD2 
0.7435 0.2565 

brine.log.hat <- predict(brine.log.lda)

brine.log.hat

$class
 [1] 2 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3
Levels: 1 2 3

$posterior
              1            2            3
1  2.312733e-01 7.627845e-01 5.942270e-03
2  9.488842e-01 3.257237e-02 1.854347e-02
3  8.453057e-01 9.482540e-04 1.537461e-01
4  9.990242e-01 8.794725e-04 9.632578e-05
5  9.965920e-01 2.849903e-03 5.581176e-04
6  9.984987e-01 1.845534e-05 1.482872e-03
7  8.676660e-01 7.666611e-06 1.323263e-01
8  4.938019e-03 9.949035e-01 1.584755e-04
9  4.356152e-03 9.956351e-01 8.770078e-06
10 2.545287e-05 9.999439e-01 3.066264e-05
11 2.081510e-02 9.791728e-01 1.210748e-05
12 1.097540e-03 9.989023e-01 1.455693e-07
13 1.440307e-02 9.854613e-01 1.356671e-04
14 4.359641e-01 2.367602e-03 5.616683e-01
15 6.169265e-02 1.540353e-04 9.381533e-01
16 7.500357e-04 4.706701e-09 9.992500e-01
17 1.430433e-03 1.095281e-06 9.985685e-01
18 2.549733e-04 3.225658e-07 9.997447e-01
19 6.433759e-02 8.576694e-03 9.270857e-01

$x
          LD1        LD2
1  -1.1576284 -0.1998499
2  -0.1846803  0.6655823
3   1.0179998  0.6827867
4  -0.3939366  2.6798084
5  -0.3167164  2.0188002
6   1.0061340  2.6434491
7   2.0725443  1.5714400
8  -2.0387449 -0.9731745
9  -2.6054261 -0.2774844
10 -2.5191350 -2.8304663
11 -2.4915044  0.3194247
12 -3.4448401  0.1869864
13 -2.0343204 -0.4674925
14  1.0441237 -0.0991014
15  1.6987023 -0.6036252
16  3.9138884 -0.7211078
17  2.7083649 -1.3896956
18  2.9310268 -1.9243611
19  0.7941483 -1.2819190


apply(brine.log.hat$posterior, MARGIN=1, FUN=max)
        1         2         3         4         5         6         7         8 
0.7627845 0.9488842 0.8453057 0.9990242 0.9965920 0.9984987 0.8676660 0.9949035 
        9        10        11        12        13        14        15        16 
0.9956351 0.9999439 0.9791728 0.9989023 0.9854613 0.5616683 0.9381533 0.9992500 
       17        18        19 
0.9985685 0.9997447 0.9270857 

plot(brine.log.lda)
plot(brine.log.lda, dimen=1, type="both")
tab <- table(brine.log$GROUP, brine.log.hat$class)
tab
   
    1 2 3
  1 6 1 0
  2 0 6 0
  3 0 0 6
sum(tab[row(tab) == col(tab)]) / sum(tab)
[1] 0.9473684

brine.log.lda <- lda(GROUP ~ HCO3 + SO4 + Cl + Ca + Mg + Na, data=brine.log, CV=TRUE)
tab <- table(brine.log$GROUP, brine.log.lda$class)

tab
   
    1 2 3
  1 6 1 0
  2 1 4 1
  3 1 0 5
sum(tab[row(tab) == col(tab)]) / sum(tab)
[1] 0.7894737