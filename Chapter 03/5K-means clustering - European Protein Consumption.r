

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

 protein = read.csv("d:/Europenaprotein.csv",header=T)
  head(protein)
 
 set.seed(123456789)
 groupMeat <- kmeans(protein[,c("WhiteMeat","RedMeat")], centers=3, nstart=10)
 
 groupMeat
  
 o=order(groupMeat$cluster)
 
 data.frame(protein$Country[o],groupMeat$cluster[o])
 
 plot(protein$Red, protein$White, type="n", xlim=c(3,19), xlab="Red Meat", ylab="White Meat")
 text(x=protein$Red, y=protein$White, labels=protein$Country,col=groupMeat$cluster+1)
 
 
 set.seed(123456789)
 groupProtein <- kmeans(protein[,-1], centers=7, nstart=10)
 o=order(groupProtein$cluster)
 data.frame(protein$Country[o],groupProtein$cluster[o])

 library(cluster)
 clusplot(protein[,-1], groupProtein$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)
 foodagg=agnes(protein,diss=FALSE,metric="euclidian")
 foodagg
 plot(foodagg, main='Dendrogram')

 groups <- cutree(foodagg, k=4)
 rect.hclust(foodagg, k=4, border="red") 