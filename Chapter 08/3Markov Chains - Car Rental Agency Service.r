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

 install.packages("markovchain")
 library(markovchain)
 
 RentalStates <- c("Downtown", "East", "West")
 
 RentalStates
 
 RentalTransitionMatrix <- matrix(c(0.3, 0.3, 0.4, 
                                     0.4, 0.4, 0.2, 
                                     0.5, 0.3, 0.2),
                                   byrow = T, nrow = 3, dimnames = list(RentalStates, RentalStates))
 
 
 RentalTransitionMatrix
 
 mcRental <- new("markovchain", states = RentalStates, byrow = T, transitionMatrix = RentalTransitionMatrix, name = "Rental Cars")
 
 mcRental
 
 mcRental[2]
 
 plot(mcRental)
 
 transitionProbability(mcRental, "East", "West")
 
 x <- 0.3 * 0.3
 y <- 0.3 * 0.4
 z <- 0.4 * 0.5
 x + y + z
 
 mcRental ^ 2
 
 mcRental^20
 
 mcRental ^ 30

 70 * steadyStates(mcRental)
 
 summary(mcRental)
 
 conditionalDistribution(mcRental, "Downtown")
 
 conditionalDistribution(mcRental, "West")
 
 conditionalDistribution(mcRental, "East")
