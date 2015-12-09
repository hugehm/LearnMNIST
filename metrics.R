metrics <- function(predictedLabels,testLabels){
  TP <- 0*1:10  # True Positive
  TN <- 0*1:10  # True Negative
  FP <- 0*1:10  # Fasle Positive
  FN <- 0*1:10  # False Negative
  for (i in 1:10){
    predictedLabelClass <- predictedLabels==(i-1)
    testLabelsClass <- testLabels==(i-1)
    TP[i] <- sum((predictedLabelClass+testLabelsClass)==2)
    TN[i] <- sum((predictedLabelClass+testLabelsClass)==0)
    FP[i] <- sum((predictedLabelClass-testLabelsClass)==1)
    FN[i] <- sum((predictedLabelClass-testLabelsClass)==-1)
    plot(roc(as.numeric(testLabelsClass), as.numeric(predictedLabelClass)))
  }
  # Calculate error metrics
  accuracy <- (TP+TN)/(TP+TN+FP+FN)
  precision <- TP/(TP+FP)
  specificity <- TP/(TP+FN)
  fmeasure <- 2*TP/(2*TP+FP+FN)
  FDR <- FP/(FP+TP)
  
  print(accuracy)
  print(precision)
  print(specificity)
  print(fmeasure)
  print(FDR)
}