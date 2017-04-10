
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

 Mathtest = read.table("d:/math test.txt",header=T)
 dist.items <- dist(Mathtest[,-1], method='euclidean')^2
 dist.items

 dist.items.2 <- dist(Mathtest[,-1], method='binary')
 dist.items.2 

 dist.items.3 <- dist(1 - Mathtest[,-1], method='binary')
 dist.items.3

 items.complete.link <- hclust(dist.items, method='complete')
 items.complete.link
 
 plot(items.complete.link, labels=Mathtest[,1], ylab="Distance")
 
 items.sing.link <- hclust(dist.items, method='single')
 items.sing.link

 plot(items.sing.link, labels=Mathtest[,1], ylab="Distance")
 
 library(cluster)
 my.k.choices <- 2:8
 
 avg.sil.width <- rep(0, times=length(my.k.choices))
 for (ii in (1:length(my.k.choices)) ){
     avg.sil.width[ii] <- pam(dist.items, k=my.k.choices[ii])$silinfo$avg.width
 }
 print( cbind(my.k.choices, avg.sil.width) )

 items.kmed.2 <- pam(dist.items, k=2, diss=T)
 items.kmed.2

 items.2.clust <- lapply(1:2, function(nc) Mathtest[,1][items.kmed.2$clustering==nc])  
 items.2.clust 
 
 items.kmed.3 <- pam(dist.items, k=3, diss=T)
 items.kmed.3
 
 items.3.clust <- lapply(1:3, function(nc) Mathtest[,1][items.kmed.3$clustering==nc])  
 items.3.clust 