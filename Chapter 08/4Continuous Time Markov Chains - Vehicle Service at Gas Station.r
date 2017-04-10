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

 install.packages("simmer")
 library(simmer)
 library(ggplot2)
 
 ArrivalRate <- 3/20
 
 ArrivalRate

 ServiceRate <- c(1/8, 1/3)

 ServiceRate
 
 p <- 0.75
 
 TransitionMatrix <- matrix(c(1,   ServiceRate[1],     0,
                               1,   -ArrivalRate,       (1-p)*ArrivalRate,
                               1,   ServiceRate[2],     -ServiceRate[2]), byrow=T, ncol=3)
 
 TransitionMatrix
 
 B <- c(1, 0, 0)
 
 P <- solve(t(A), B)
 
 P
 
 Resolution <- sum(P * c(1, 0, 1))
 
 Resolution
 
 set.seed(1234)
 
 option.1 <- function(t) {
      car <- create_trajectory() %>%
          seize("pump", amount=1) %>%
          timeout(function() rexp(1, ServiceRate[1])) %>%
          release("pump", amount=1)
      
      motorcycle <- create_trajectory() %>%
          seize("pump", amount=1) %>%
          timeout(function() rexp(1, ServiceRate[2])) %>%
          release("pump", amount=1)
      
      simmer() %>%
          add_resource("pump", capacity=1, queue_size=0) %>%
          add_generator("car", car, function() rexp(1, p*ArrivalRate)) %>%
          add_generator("motorcycle", motorcycle, function() rexp(1, (1-p)*ArrivalRate)) %>%
          run(until=t)
  }
 
 
 
 option.2 <- function(t) {
      vehicle <- create_trajectory() %>%
          seize("pump", amount=1) %>%
          branch(function() sample(c(1, 2), 1, prob=c(p, 1-p)), merge=c(T, T),
                 create_trajectory("car") %>%
                     timeout(function() rexp(1, ServiceRate[1])),
                 create_trajectory("motorcycle") %>%
                     timeout(function() rexp(1, ServiceRate[2]))) %>%
          release("pump", amount=1)
      
      simmer() %>%
          add_resource("pump", capacity=1, queue_size=0) %>%
          add_generator("vehicle", vehicle, function() rexp(1, ArrivalRate)) %>%
          run(until=t)
  }
 
 option.3 <- function(t) {
      vehicle <- create_trajectory() %>%
          seize("pump", amount=1) %>%
          timeout(function() {
              if (runif(1) < p) rexp(1, ServiceRate[1])  
              else rexp(1, ServiceRate[2])               
          }) %>%
          release("pump", amount=1)
      
      simmer() %>%
          add_resource("pump", capacity=1, queue_size=0) %>%
          add_generator("vehicle", vehicle, function() rexp(1, ArrivalRate)) %>%
          run(until=t)
  }
 
 gas.station <- option.3(5000)
 
 graph <- plot_resource_usage(gas.station, "pump", items="system")
 
 graph + geom_hline(yintercept = Resolution)
