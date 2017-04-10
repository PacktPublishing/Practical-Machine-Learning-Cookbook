 library(cluster)
 
 food.energycontent <- read.table("d:/foodstuffs.txt", header=T)
 head(food.energycontent)
  Food Energy Protein Fat Calcium Iron
1   BB    340      20  28       9  2.6
2   HR    245      21  17       9  2.7
3   BR    420      15  39       7  2.0
4   BS    375      19  32       9  2.5
5   BC    180      22  10      17  3.7
6   CB    115      20   3       8  1.4

 str(food.energycontent)

 standard.deviation <- apply(food.energycontent[,-1], 2, sd)
 standard.deviation

 foodergycnt.stddev <- sweep(food.energycontent[,-1],2,standard.deviation,FUN="/") 
 foodergycnt.stddev
 
 food.5cluster <- kmeans(foodergycnt.stddev, centers=5, iter.max=100, nstart=25)
 food.5cluster
     
 food.4cluster <- kmeans(foodergycnt.stddev, centers=4, iter.max=100, nstart=25)
 food.4cluster
 
 food.4cluster$cluster

 food.4cluster.clust <- lapply(1:4, function(nc) protein[food.4cluster$cluster==nc])  
 food.4cluster.clust

 pairs(food.energycontent[,-1], panel=function(x,y) text(x,y,food.4cluster$cluster))
 
 food.pc <- princomp(food.energycontent[,-1],cor=T)
 my.color.vector <- rep("green", times=nrow(food.energycontent))
 my.color.vector[food.4cluster$cluster==2] <- "blue"
 my.color.vector[food.4cluster$cluster==3] <- "red"
 my.color.vector[food.4cluster$cluster==4] <- "orange"
 par(pty="s")
 plot(food.pc$scores[,1], food.pc$scores[,2], ylim=range(food.pc$scores[,1]), 
      xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
 text(food.pc$scores[,1], food.pc$scores[,2], labels=Food, cex=0.7, lwd=2,
      col=my.color.vector)