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
Type 'q()' to quit R.

[Workspace loaded from ~/.RData]
 install.packages("glmnet")

 library(ggplot2)
 library(glmnet)

 datafile <- file.path("d:","epic_recipes.txt")
 
 recipes_data <- read.table(datafile, fill=TRUE, col.names=1:max(count.fields(datafile)), na.strings=c("", "NA"), stringsAsFactors = FALSE)
 
 
 agg <- aggregate(recipes_data[,-1], by=list(recipes_data[,1]), paste, collapse=",")
 
 agg$combined <- apply(agg[,2:ncol(agg)], 1, paste, collapse=",")
 
 agg$combined <- gsub(",NA","",agg$combined) 
 
 
 
 cuisines <- as.data.frame(table(recipes_data[,1]))
 
 cuisines
 
 
 ingredients_freq <- lapply(lapply(strsplit(a$combined,","), table), as.data.frame) 

 
 names(ingredients_freq) <- agg[,1]

 proportion <- lapply(seq_along(ingredients_freq), function(i) {
     colnames(ingredients_freq[[i]])[2] <- names(ingredients_freq)[i]
     ingredients_freq[[i]][,2] <- ingredients_freq[[i]][,2]/cuisines[i,2] 
     ingredients_freq[[i]]}
 )
 

 names(proportion) <- a[,1]
 

 final <- Reduce(function(...) merge(..., all=TRUE, by="Var1"), proportion)
 

 row.names(final) <- final[,1]

 final <- final[,-1]

 final[is.na(final)] <- 0

 prop_matrix <- t(final)

 s <- sort(apply(prop_matrix, 2, sd), decreasing=TRUE)
 
 final_imp<- scale(subset(prop_matrix, select=names(which(s > 0.1))))

 heatmap.2(final_imp, trace="none", margins = c(6,11), col=topo.colors(7), key=TRUE, key.title=NA, keysize=1.2, density.info="none") 


 pca_computation <- princomp(final_imp) 
 
 
 pca_computation


 biplot(pca_computation, pc.biplot=TRUE, col=c("black","red"), cex=c(0.9,0.8), xlim=c(-2.5,2.5), xlab="PC1, 39.7%", ylab="PC2, 24.5%") 