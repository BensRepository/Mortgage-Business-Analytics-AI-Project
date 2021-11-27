# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
# COM3018 PRATICAL BUSINESS ANALYTICS
#
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# DATE: 29th November 2021
# VERSION: v1.00
# AUTHOR: Group CHAMPS
#
# UPDATE
# 1.00      29/11/2021    Initial Version
# ************************************************


# ************************************************
# NimportanceOfInput() :
#
# Prints and Plots out the importance of the inputs for classifier 
#
# INPUT     : list           - classifier       - Classifer from training model
#             character      - title            - Title for plots
#             character      - model            - Name of model
#
# OUTPUT    : None                              - None
#
# Requires library: c50
#                   randomforest
#                   h2o
#
# Reference: Code obtained from LAB 4
# ************************************************
NimportanceOfInput<-function(classifier, title, model=NA){
  
  if (!is.na(model)){
    
    if (model == "decisionTree"){
      importance <- C50::C5imp(classifier, metric = "usage")
    }
    
    if (model == "randomForest"){
      importance <- randomForest::importance(classifier, scale=TRUE, type=1)
      importance <- data.frame(importance)
    }
    
    if (model == "neuralNetwork"){
      importance <- as.data.frame(h2o::h2o.varimp(classifier))
      row.names(importance) <- importance$variable
      importance <- subset(importance, select=scaled_importance)*100
      importance <- data.frame(importance)
    }
    
    colnames(importance) <- "Usage"
    importance <- importance[order(importance$Usage, decreasing=TRUE), , drop=FALSE]
    
    colnames(importance) <- paste(colnames(importance), "of", title)
    print(formattable::formattable(importance))
    
    # Increase margins
    par(mar=c(5, 15, 5, 2))
    
    barplot(t(importance), col="pink",
            border = 0, cex.names =0.7,
            main=title, horiz=T, las=1)
    
    # Default margins 
    par(mar=c(5.1, 4.1, 4.1, 2.1))
    
  }
} 


# ************************************************
# NtestPredictOutput() :
#
# Predict the output of the input from the testing dataset
#
# INPUT    : object         - classifer        - Training classifier
#            data Frame     - testingDataset   - Testing dataset to evaluate
#            character      - outputField      - Name of output field
#            character      - model            - Type of model
#
# OUTPUT   : vector         - testPredict      - Vector of probability from class 1
#
# Requires library: h2o
#
# Reference: Code obtained from LAB 4
# ************************************************
NtestPredictOutput<-function(classifier, testingDataset, outputField, model=NA){
  
  # Probabilities for class 1
  if (!is.na(model)){
    
    if (model == "tree"){
      
      testPredicted <- predict(classifier, testingDataset$INPUT, type="prob")
      testPredicted <- testPredicted[,2]
      
    }
    
    if (model == "neuralNetwork"){
      
      # Output: if a factor, then assume classification, otherwise regression
      testingDataset$ALL[outputField] <- lapply(testingDataset$ALL[outputField] , factor)
      test_h2o <- as.h2o(testingDataset$ALL, destination_frame = "testdata")
      
      testPredicted <- h2o::h2o.predict(classifier, test_h2o)
      testPredicted <- as.vector(testPredicted$p1)
    
    }
    
    return(testPredicted)
    
  }
}


# ************************************************
# NPLOTROC() :
#
# Plots ROC curve from testingdata predictions and returns threshold
#
# INPUT    : vector         - testPredicted    - Predicted probbilities
#            vector         - testExpected     - Expected result
#            character      - title            - Title for plot
#
# OUTPUT   : double         - threshold        - Calculated threshold value
#
# Requires library: pROC
#
# Reference: Code obtained from LAB 4
# ************************************************
NPLOTROC<-function(testPredicted, testExpected, title){
  
  roc <- pROC::roc(response=testExpected,
                   predictor=testPredicted,
                   plot=TRUE,
                   auc=TRUE,
                   auc.polygon=TRUE,
                   percent=TRUE,
                   grid=TRUE,
                   print.auc=TRUE,
                   main=paste("ROC for Classifier Model",title),
                   xlab="Specificity (TNR) %",
                   ylab="Sensitivity (TPR) %")
  
  # Selects the minimum threshold based on distance
  # sqrt((1 − sensitivity)^2+ (1 − specificity)^2) 
  # i.e sqrt( FNR^2 + FPR^2 )
  analysis<-coords(roc, x="best",transpose = FALSE,
                   best.method="closest.topleft",
                   ret=c("threshold",
                         "specificity",
                         "sensitivity"))
  
  # Add crossing lines
  abline(h=analysis["sensitivity"], col="blue",lty=3,lwd=2)
  abline(v=analysis["specificity"], col="blue",lty=3,lwd=2)
  
  # Precentage values
  annotate <- paste("Threshold: ",round(analysis["threshold"],digits=4L),
                    " TPR %: ", round(analysis["sensitivity"],digits=2L),
                    " TNR %: ", round(analysis["specificity"],digits=2L), sep="")
  
  text(x=analysis["specificity"],
       y=analysis["sensitivity"], adj = c(-0.2,2), cex=1,
       col="blue", annotate)
  
  return(round(analysis["threshold"], digits=4L)[1,1])
  
}


# ************************************************
# NcalcConfusion() :
#
# Calculate a confusion matrix for 2-class classifier
# 
# INPUT      : vector - predictedClass - {0,1}, Predicted outcome from each row (labels)
#              vector - expectedClass  - {0,1}, Expected outcome from each row (labels)
#
# OUTPUT: A list with the  entries from NcalcMeasures()
#
#                    ACTUAL
#               ------------------
# PREDICTED     GOOD=1   |  BAD=0
#               ------------------
#     GOOD=1      TP     |    FP
#               ==================
#     BAD=0       FN     |    TN
#
# Reference: Code obtained from LAB 4
# ************************************************
NcalcConfusion<-function(predictedClass, expectedClass){
  
  confusion <- table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))
  
  TP <- as.double(confusion[2,2])
  FN <- as.double(confusion[1,2])
  FP <- as.double(confusion[2,1])
  TN <- as.double(confusion[1,1])
  
  return(NcalcMeasures(TP,FN,FP,TN))
  
} 


# ************************************************
# NcalcMeasures() :
#
# Evaluation measures for confusion matrix
#
# INPUT  : numeric  - TP, FN, FP, TN
#
# OUTPUT : A list with the following entries:
#        TP        - double - True Positive records
#        FP        - double - False Positive records
#        TN        - double - True Negative records
#        FN        - double - False Negative records
#        accuracy  - double - accuracy measure
#        pgood     - double - precision for predicted 1s measure
#        pbad      - double - precision for predicted 0s measure
#        FPR       - double - FPR measure 
#        FNR       - double - FNR measure
#        TPR       - double - FPR measure 
#        TNR       - double - TNR measure
#        MCC       - double - Matthew's Correlation Coeficient
#
# Reference: Code obtained from LAB 4
# ************************************************
NcalcMeasures<-function(TP,FN,FP,TN){
  
  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  
                  "accuracy"=100.0*((TP+TN)/(TP+FP+FN+TN)),
                  
                  "pgood"=   100.0*(TP/(TP+FP)),
                  "pbad"=    100.0*(TN/(FN+TN)),

                  # TPR and FNR are complementary measures
                  "TPR"=     100.0*(TP/(TP+FN)),
                  "FNR"=     100.0*(FN/(TP+FN)),

                  # TNR and FPR are complementary measures
                  "TNR"=     100.0*(TN/(FP+TN)),
                  "FPR"=     100.0*(FP/(FP+TN)),
                  
                  "MCC"=     ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  )
  return(retList)
}


# ************************************************
# Nevaluate() :
#
# Plots ROC curve from testingdata predictions and returns threshold
#
# INPUT    : object         - classifier       - Training classifier
#            data frame     - testingDataset   - Testing dataset
#            character      - outputField      - Name of output field
#            character      - model            - Type of model
#            character      - title            - Title for plot
#
# OUTPUT   : A list with the  entries from NcalcMeasures
# ************************************************
Nevaluate<-function(classifier, testingDataset, outputField, model=NA, title){
  
  # Predict testing data with classifier
  testPredicted <- NtestPredictOutput(classifier, testingDataset, outputField, model)
  testExpected  <- testingDataset$OUTPUT
  
  # Plot ROC curve and return the threshold
  threshold <- NPLOTROC(testPredicted, testExpected, title)
  
  # Check if the predicted probability result is above or below threshold
  testPredicted <- ifelse(testPredicted < threshold, 0, 1)
  
  # Calculate measures
  results <- NcalcConfusion(expectedClass=testExpected,
                            predictedClass=testPredicted)
  
  results$threshold <- threshold
  
  return(results)
}


# ************************************************
# NPLOTandSummary() :
#
# Plot and Summary for Neural Networks
#
# INPUT    : object         - classifier       - Training classifier
#            character      - title            - Title of classifier
#
# OUTPUT   : None                              - None
# ************************************************
NPLOTandSummary<-function(classifier, title){
  
  summary(classifier)
  plot(classifier, sub=title)
}

