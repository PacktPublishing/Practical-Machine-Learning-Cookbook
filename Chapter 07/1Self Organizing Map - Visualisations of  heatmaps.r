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

 library(kohonen)
 
 training_frame <- data[, c(2,4,5,8)]

 training_matrix <- as.matrix(scale(training_frame))
 
 training_matrix
             zn       chas         nox           dis
1    0.28454827 -0.2723291 -0.14407485  0.1400749840
2   -0.48724019 -0.2723291 -0.73953036  0.5566090496
3   -0.48724019 -0.2723291 -0.73953036  0.5566090496
4   -0.48724019 -0.2723291 -0.83445805  1.0766711351
5   -0.48724019 -0.2723291 -0.83445805  1.0766711351
6   -0.48724019 -0.2723291 -0.83445805  1.0766711351
7    0.04872402 -0.2723291 -0.26489191  0.8384142195
8    0.04872402 -0.2723291 -0.26489191  1.0236248974
9    0.04872402 -0.2723291 -0.26489191  1.0861216287
10   0.04872402 -0.2723291 -0.26489191  1.3283202075
11   0.04872402 -0.2723291 -0.26489191  1.2117799501
12   0.04872402 -0.2723291 -0.26489191  1.1547920492
13   0.04872402 -0.2723291 -0.26489191  0.7863652700
14  -0.48724019 -0.2723291 -0.14407485  0.4333252240
15  -0.48724019 -0.2723291 -0.14407485  0.3166899868
16  -0.48724019 -0.2723291 -0.14407485  0.3341187865
17  -0.48724019 -0.2723291 -0.14407485  0.3341187865
18  -0.48724019 -0.2723291 -0.14407485  0.2198105553
19  -0.48724019 -0.2723291 -0.14407485  0.0006920764


 
 som_grid <- somgrid(xdim = 20, ydim=20, topo="hexagonal")

 
 som_model <- som(training_matrix, 
                  grid=som_grid, 
                  rlen=1000, 
                  alpha=c(0.05,0.01), 
                  keep.data = TRUE,
                  n.hood="circular")

 plot(som_model, main ="Training Progress", type="changes", col = "red")
 
 
 
 plot(som_model, main ="Node Count", type="count")
 
 
 plot(som_model, main ="Neighbour Distances", type="dist.neighbours")

 
 plot(som_model, type="codes")

 plot(som_model, type = "property", property = som_model$codes[,4], main=names(som_model$data)[4])
