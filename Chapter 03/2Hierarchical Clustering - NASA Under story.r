
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

 NASA = read.csv("d:/NASAUnderstory.csv",header=T)
 NASA.lab=NASA$Labels
 NASA.lab
 NASA=NASA[,-32]
 NASA
 
 NASAscale<-scale(NASA[,3:31])
 NASAscale
 
 rownames(NASAscale)=as.factor(NASA$Overstory.Species)
 rownames(NASAscale)
 
 dist1<-dist(NASAscale, method="euclidean")
 
 clust1<-hclust(dist1,method="ward.D")
 clust1
 
 plot(clust1,labels= NASA[,2], cex=0.5, xlab="",ylab="Distance",main="Clustering for NASA Understory Data")
 
 rect.hclust(clust1,k=2)
 
 cuts=cutree(clust1,k=2)
 
 cuts


 library(vegan)
 
 dist1<-vegdist(NASA[,3:31], method="jaccard", upper=T)
 clust1<-hclust(dist1,method="ward.D")
 
 clust1

 plot(clust1,labels= NASA[,2], cex=0.5, xlab="",ylab="Distance",main="Clustering for NASA Understory Data")
 rect.hclust(clust1,k=2)
 cuts=cutree(clust1,k=2)
 cuts


 clusplot(NASA, cuts, color=TRUE, shade=TRUE, labels=2, lines=0,
          main="NASA Two Cluster Plot, Ward's Method, First two PC")

 library(fpc)
 NASAtrans=t(NASAscale)


 dist1<-dist(NASAtrans, method="minkowski", p=3)


 clust1<-hclust(dist1,method="ward.D")
 clust1

 plot(clust1,labels= NASA.lab[1:29], cex=1, xlab="",ylab="Distance",main="Clustering for NASA Understory Data")
 rect.hclust(clust1,k=3)
 
 

