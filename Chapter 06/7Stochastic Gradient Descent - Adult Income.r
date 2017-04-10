R version 3.3.0 (2016-05-03) -- "Supposedly Educational"
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

 library("klaR")
 library("caret")
 library ("stringr")

 allData <- read.table('d:/adult.txt', header = FALSE) 

 str(allData)

 labels <- as.factor(allData[,15])

 allFeatures <- allData[,-c(15)]

 head(allFeatures)

 continuousFeatures <- scale(continuousFeatures)


 head(continuousFeatures)

 labels.n = rep(0,length(labels))
 labels.n[labels==" <=50K"] = -1
 labels.n[labels==" 50K"] = 1
 labels = labels.n
 rm(labels.n)


 trainingData <- createDataPartition(y=labels, p=.8, list=FALSE)

 dim(trainingData)

 remainingLabels <- labels[-trainingData]
 remainingFeatures <- continuousFeatures[-trainingData,]

 testingData <- createDataPartition(y=remainingLabels, p=.5, list=FALSE)
 testingLabels <- remainingLabels[testingData]
 testingFeatures <- remainingFeatures[testingData,]

 validationLabels <- remainingLabels[-testingData]
 validationFeatures <- remainingFeatures[-testingData,]


 getAccuracy <- function(a,b,features,labels){
     estFxn = features %*% a + b
     predictedLabels = rep(0,length(labels))
     predictedLabels [estFxn < 0] = -1 
     predictedLabels [estFxn = 0] = 1 
     
     return(sum(predictedLabels == labels) / length(labels))
 }
 
 numEpochs = 100
 numStepsPerEpoch = 500
 nStepsPerPlot = 30
 evalidationSetSize = 50
 c1 = 0.01
 c2 = 50

 lambda_vals = c(0.001, 0.01, 0.1, 1)
 bestAccuracy = 0


 str(lambda_vals)

 accMat <- matrix(NA, nrow = (numStepsPerEpoch/nStepsPerPlot)*numEpochs+1, ncol = length(lambda_vals))
 accMatv <- matrix(NA, nrow = (numStepsPerEpoch/nStepsPerPlot)*numEpochs+1, ncol = length(lambda_vals))
 
 
 for(i in 1:4){
  lambda = lambda_vals[i]
  accMatRow = 1
  accMatCol = i
 
  a = rep(0,ncol(continuousFeatures))
  b = 0
 
  stepIndex = 0
 
  for (e in 1:numEpochs){
 
    etrainingData <- createDataPartition(y=trainingLabels, p=(1 - evalidationSetSize/length(trainingLabels)), list=FALSE)
 
    etrainingFeatures <- trainingFeatures[etrainingData,]
    etrainingLabels <- trainingLabels[etrainingData]
 
    evalidationFeatures <- trainingFeatures[-etrainingData,]
    evalidationLabels <- trainingLabels[-etrainingData]
 
    steplength = 1 / (e*c1 + c2)
 
    for (step in 1:numStepsPerEpoch){
      stepIndex = stepIndex+1
      index = sample.int(nrow(etrainingFeatures),1)
      xk = etrainingFeatures[index,]
      yk = etrainingLabels[index]
 
      costfxn = yk * (a %*% xk + b)
 
      if(costfxn >= 1){
 
        a_dir = lambda * a
        a = a - steplength * a_dir
         
      } else {
 
        a_dir = (lambda * a) - (yk * xk)
        a = a - steplength * a_dir
        b_dir = -yk
        b = b - (steplength * b_dir)
 
      }
 
 
      if (stepIndex %>% nStepsPerPlot == 1){#30){
        accMat[accMatRow,accMatCol] = getAccuracy(a,b,evalidationFeatures,evalidationLabels)
        accMatv[accMatRow,accMatCol] = getAccuracy(a,b,validationFeatures,validationLabels)
        accMatRow = accMatRow + 1
      }
 
    }
 
  }
 
  tempAccuracy = getAccuracy(a,b,validationFeatures,validationLabels)
  print(str_c("tempAcc = ", tempAccuracy," and bestAcc = ", bestAccuracy) )
  if(tempAccuracy > bestAccuracy){
    bestAccuracy = tempAccuracy
    best_a = a
    best_b = b
    best_lambdaIndex = i
  }
 
}

 getAccuracy(best_a,best_b, testingFeatures, testingLabels)

 colors = c("red","blue","green","black")

 xaxislabel = "Step"

 yaxislabels = c("Accuracy on Randomized Epoch Validation Set","Accuracy on Validation Set")

 title="Accuracy as a Function of Step and Lambda"

 ylims=c(0,1)

 stepValues = seq(1,15000,length=500)

 mats =  list(accMat,accMatv)

 for(j in 1:length(mats)){
 
  mat = mats[[j]]
 
  for(i in 1:4){
 
   if(i == 1){
     plot(stepValues, mat[1:500,i], type = "l",xlim=c(0, 15000), ylim=ylims,
         col=colors[i],xlab=xaxislabel,ylab=yaxislabels[j],main=title)
    } else{
      lines(stepValues, mat[1:500,i], type = "l",xlim=c(0, 15000), ylim=ylims,
          col=colors[i],xlab=xaxislabel,ylab=yaxislabels[j],main=title)
    }
   Sys.sleep(1)
  }
  legend(x=10000,y=.5,legend=c("lambda=.001","lambda=.01","lambda=.1","lambda=1"),fill=colors)
 
}
