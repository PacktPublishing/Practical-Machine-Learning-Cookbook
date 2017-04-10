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
Type 'q()' to quit R

 install.packages("rgl")
 install.packages("RColorBrewer")
 install.packages("scales")


 library(rgl)
 library(RColorBrewer)
 library(scales)

 delta <- read.csv(file="d:/delta.csv", header=T, sep=",", row.names=1)
 
 str(delta)

 plot(delta[,16:22], main = "Aircraft Physical Characteristics", col = "red")

 principal_comp_analysis <- princomp(delta)
 
 principal_comp_analysis

 plot(principal_comp_analysis, main ="Principal Components Analysis of Raw Data", col ="blue")


 loadings(principal_comp_analysis)

 mar <- par()$mar
 par(mar=mar+c(0,5,0,0))
 
 barplot(sapply(delta, var), horiz=T, las=1, cex.names=0.8, main = "Regular Scaling of Variance", col = "Red", xlab = "Variance")

 
 barplot(sapply(delta, var), horiz=T, las=1, cex.names=0.8, log='x', main = "Logarithmic  Scaling of Variance", col = "Blue", xlab = "Variance")

 
 par(mar=mar)

 
 delta2 <- data.frame(scale(delta))
 
 
 plot(sapply(delta2, var), main = "Variances Across Different Variables", ylab = "Variances")

 principal_comp_analysis <- princomp(delta2)
 

 
 plot(principal_comp_analysis, main ="Principal Components Analysis of Scaled Data", col ="red")
 

 
 plot(principal_comp_analysis, type='l', main ="Principal Components Analysis of Scaled Data")

 
 
 summary(principal_comp_analysis)

 
 principal_comp_vectors <- prcomp(delta2)

 comp <- data.frame(principal_comp_vectors$x[,1:4])
 
 k_means <- kmeans(comp, 4, nstart=25, iter.max=1000)
 

 
 palette(alpha(brewer.pal(9,'Set1'), 0.5))
 
 plot(comp, col=k_means$clust, pch=16)
 

 
 plot3d(comp$PC1, comp$PC2, comp$PC3, col=k_means$clust)
 
 
 plot3d(comp$PC1, comp$PC3, comp$PC4, col=k_means$clust)
 
 sort(table(k_means$clust))
 
 
 clust <- names(sort(table(k_means$clust)))
 
 row.names(delta[k_means$clust==clust[1],])

 row.names(delta[k_means$clust==clust[2],])
                

 row.names(delta[k_means$clust==clust[3],])

 row.names(delta[k_means$clust==clust[4],])
