 install.packages(c("RColorBrewer", "cluster", "pvclust", "xtable", "plyr"))
 library(RColorBrewer)
 library(cluster)
 library(pvclust)
 library(xtable)
 library(plyr)


 GSE4051_data <- read.csv("d:/GSE4051_data.csv", header = TRUE)
 str(GSE4051_data, max.level = 0)
'data.frame':	29949 obs. of  39 variables:


 GSE4051_design <- read.csv("d:/GSE4051_design.csv", header = TRUE)
 str(GSE4051_design)
'data.frame':	39 obs. of  4 variables:
 $ sidChar : Factor w/ 39 levels "Sample_1","Sample_10",..: 13 14 15 16 8 9 36 17 18 19 ...
 $ sidNum  : int  20 21 22 23 16 17 6 24 25 26 ...
 $ devStage: Factor w/ 5 levels "4_weeks","E16",..: 2 2 2 2 2 2 2 4 4 4 ...
 $ gType   : Factor w/ 2 levels "NrlKO","wt": 2 2 2 2 1 1 1 2 2 2 ...


 trans_GSE4051_data <- t(scale(t(GSE4051_data)))
 str(trans_GSE4051_data, max.level = 0, give.attr = FALSE)
 num [1:29949, 1:39] 0.0838 0.1758 0.7797 -0.3196 0.8358 ...


 round(data.frame(avgBefore = rowMeans(head(GSE4051_data)),
                 avgAfter = rowMeans(head(trans_GSE4051_data)),
                 varBefore = apply(head(GSE4051_data), 1, var),
                 varAfter = apply(head(trans_GSE4051_data), 1, var)), 2)
  avgBefore avgAfter varBefore varAfter
1      7.22        0      0.02        1
2      9.37        0      0.35        1
3      9.70        0      0.15        1
4      8.42        0      0.03        1
5      8.47        0      0.02        1
6      9.67        0      0.03        1


 pair_dist_GSE4051_data <- dist(t(trans_GSE4051_data), method = 'euclidean')


 GSE4051_design$group <- with(GSE4051_design, interaction(gType, devStage))
 summary(GSE4051_design$group)
NrlKO.4_weeks    wt.4_weeks     NrlKO.E16        wt.E16     NrlKO.P10        wt.P10      NrlKO.P2         wt.P2      NrlKO.P6 
            4             4             3             4             4             4             4             4             4 
        wt.P6 
            4 


 pr.hc.single <- hclust(pair_dist_GSE4051_data, method = 'single')
 pr.hc.single
Call:
hclust(d = pr.dis, method = "single")

Cluster method   : single 
Distance         : euclidean 
Number of objects: 39


 pr.hc.complete <- hclust(pair_dist_GSE4051_data, method = 'complete')
 pr.hc.complete

Call:
hclust(d = pr.dis, method = "complete")

Cluster method   : complete 


 pr.hc.average <- hclust(pair_dist_GSE4051_data, method = 'average')
 pr.hc.average

Call:
hclust(d = pr.dis, method = "average")

Cluster method   : average 
Distance         : euclidean 
Number of objects: 39 


 pr.hc.ward <- hclust(pair_dist_GSE4051_data, method = 'ward.D2')
 pr.hc.ward

Call:
hclust(d = pr.dis, method = "ward.D2")

Cluster method   : ward.D2 
Distance         : euclidean 
Number of objects: 39 


Distance         : euclidean 
Number of objects: 39


 op <- par(mar = c(0,4,4,2), mfrow = c(2,2))


 plot(pr.hc.single, labels = FALSE, main = "Single Linkage Representation", xlab = "")
 
 plot(pr.hc.complete, labels = FALSE, main = "Complete Linkage Representation", xlab = "")
 
 plot(pr.hc.average, labels = FALSE, main = "Average Linkage Representation", xlab = "")
 
 plot(pr.hc.ward, labels = FALSE, main = "Ward Linkage Representation", xlab = "")
 
 par(op)

 op <- par(mar = c(1,4,4,1))
 
 plot(pr.hc.single, labels = GSE4051_design$group, cex = 0.6, main = "Single Hierarchical Cluster - 10 clusters")
 rect.hclust(pr.hc.single, k = 10)
 par(op)
 jGraysFun <- colorRampPalette(brewer.pal(n = 9, "Blues"))
 gTypeCols <- brewer.pal(9, "Spectral")[c(4,7)]
 heatmap(as.matrix(trans_GSE4051_data), Rowv = NA, col = jGraysFun(256), hclustfun = function(x) hclust(x, method = 'single'),
	scale = "none", labCol = GSE4051_design	$group, labRow = NA, margins = c(8,1), 
	ColSideColor = gTypeCols[unclass(GSE4051_design$gType)])
 legend("topright", legend = levels(GSE4051_design$gType), col = gTypeCols, lty = 1, lwd = 5, cex = 0.5)


 plot(pr.hc.complete, labels = GSE4051_design$group, cex = 0.6, main = "Complete Hierarchical Cluster - 10 clusters")
 rect.hclust(pr.hc.complete, k = 10)
 par(op)
 jGraysFun <- colorRampPalette(brewer.pal(n = 9, "Greens"))
 gTypeCols <- brewer.pal(11, "PRGn")[c(4,7)]
 heatmap(as.matrix(trans_GSE4051_data), Rowv = NA, col = jGraysFun(256), hclustfun = function(x) hclust(x, method = 'complete'), 
	scale = "none", labCol = GSE4051_design$group, labRow = NA, margins = c(8,1),
	ColSideColor = gTypeCols[unclass(GSE4051_design$gType)])
 legend("topright", legend = levels(GSE4051_design$gType), col = gTypeCols, lty = 1, lwd = 5, cex = 0.5)


 plot(pr.hc.average, labels = GSE4051_design$group, cex = 0.6, main = "Average Hierarchical Cluster - 10 clusters")
 rect.hclust(pr.hc.average, k = 10)
 jGraysFun <- colorRampPalette(brewer.pal(n = 9, "Oranges"))
 gTypeCols <- brewer.pal(9, "Oranges")[c(4,7)]
 heatmap(as.matrix(trans_GSE4051_data), Rowv = NA, col = jGraysFun(256), hclustfun = function(x) hclust(x, method = 'average'), 
	scale = "none", labCol = GSE4051_design$group, labRow = NA, margins = c(8,1), 
	ColSideColor = gTypeCols[unclass(GSE4051_design$gType)])
 legend("topright", legend = levels(GSE4051_design$gType), col = gTypeCols, lty = 1, lwd = 5, cex = 0.5)


 plot(pr.hc.ward, labels = GSE4051_design$group, cex = 0.6, main = "Ward Hierarchical Cluster - 10 clusters")
 rect.hclust(pr.hc.ward, k = 10)
 jGraysFun <- colorRampPalette(brewer.pal(n = 9, "Reds"))
 gTypeCols <- brewer.pal(9, "Reds")[c(4,7)]
 heatmap(as.matrix(trans_GSE4051_data), Rowv = NA, col = jGraysFun(256), hclustfun = function(x) hclust(x, method = 'ward.D2'), 
	scale = "none", labCol = GSE4051_design$group, labRow = NA, margins = c(8,1), 
	ColSideColor = gTypeCols[unclass(GSE4051_design$gType)])
 legend("topright", legend = levels(GSE4051_design$gType), col = gTypeCols, lty = 1, lwd = 5, cex = 0.5)



