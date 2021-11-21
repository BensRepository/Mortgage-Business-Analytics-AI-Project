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
# AUTHOR: Group Something
#
# UPDATE
# 1.00      29/11/2021    Initial Version
# ************************************************

# ************************************************
# NreadDataset() :
#
# Read a CSV file from working directory
#
# INPUT       : string - csvFilename - CSV filename
#
# OUTPUT      : data frame - contents of the CSV file
# ************************************************
NreadDataset<-function(csvFilename){
  
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)
  
  # The field names "confuse" some of the library algorithms
  # As they do not like spaces, punctuation, etc.
  # names(dataset)<-NPREPROCESSING_removePunctuation(names(dataset))
  
  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}


# ************************************************
# NcleanDataset() :
#
# Removes unwanted fields and records with NA 
#
# INPUT       : data frame - dataset read from CSV file
#
# OUTPUT      : data frame - cleaned dataset
# ************************************************
NcleanDataset<-function(dataset, fieldsToRemove){
  
  originalColumnLength <- length(dataset)
  originalRowCount <- nrow(dataset)
  
  dataset<- dataset[-fieldsToRemove]
  dataset <- na.omit(dataset)
  
  
  
  dataset <- dataset[!(dataset$action_taken_name== "Application withdrawn by applicant" |
                       dataset$action_taken_name== "Loan purchased by the institution" |
                       dataset$action_taken_name== "File closed for incompleteness" |
                       dataset$action_taken_name== "Application approved but not accepted" |
                       dataset$action_taken_name== "Preapproval request approved but not accepted" |
                       dataset$action_taken_name== "Loan purchased by the institution" |
                       dataset$action_taken_name== "Preapproval request denied by financial institution"
                       ),]
  
  cleanedColumnLength <- length(dataset)
  cleanedRowCount <- nrow(dataset)
  
  print(paste("Amount of fields before=", originalColumnLength, "after=", cleanedColumnLength))
  print(paste("Amount of records before=", originalRowCount, "after=", cleanedRowCount))
  return(dataset)
}


# ************************************************
# NPREPROCESSING_presentDataset()

# Output simple dataset field analysis results as a table in "Viewer"
# Writes an Excel file with statistcs over the fields
#
# REQUIRES: formattable
#
# INPUT          : data frame    - dataset, full dataset used for train/test
#                                - Each row is one record, each column in named
#                                - Values are not scaled or encoded
#
# OUTPUT         : None
#
# Requires the library: PerformanceAnalytics
#                       formattable
# ************************************************
NPREPROCESSING_presentDataset<-function(dataset){
  
  wb <- createWorkbook()
  
  tidyTable<-data.frame(Field=names(dataset),
                        SYMBOLIC=FALSE,
                        Categories=0,
                        HighestPercentage=0,
                        NUMERIC=FALSE,
                        Min=0.0,
                        Mean=0.0,
                        Max=0.0,
                        Skew=0.0,
                        stringsAsFactors = FALSE)
  
  for (field in 1:ncol(dataset)){
    # If it is not NUMERIC it is SYMBOLIC
    isNUMERIC <- is.numeric(dataset[,field])
    tidyTable$NUMERIC[field] <-isNUMERIC
    tidyTable$SYMBOLIC[field]<-!isNUMERIC

    
    if (isNUMERIC){
      tidyTable$Max[field]  <-round(max(dataset[,field]),2)
      tidyTable$Mean[field] <-round(mean(dataset[,field]),2)
      tidyTable$Min[field]  <-round(min(dataset[,field]),2)
      tidyTable$Skew[field] <-round(PerformanceAnalytics::skewness(dataset[,field],method="moment"),2)
      
    } else
    {
      # Amount of categories in a SYMBOLIC field
      tidyTable$Categories[field] <- length(unique(dataset[,field]))
      
      # Counts each category and calculate the percentage in a SYMBOLIC field
      categoryCount <- sapply(unique(dataset[,field]),function(x) length(which(dataset[,field]==x)))
      categoryPC <- round((categoryCount/nrow(dataset))*100,digits=0)
      
      # Stores result in a descending order
      df <- data.frame(Category=names(categoryCount), Amount=categoryCount, Percentage=categoryPC)
      df <- df[order(-df$Amount),]
      
      # Displays the category with the highest percentage
      tidyTable$HighestPercentage[field] <- paste(df[1,1],"(",df[1,3],"%)",sep="")
      
      # Saves the result in a Excel file
      addWorksheet(wb, names(dataset)[field])
      writeData(wb, names(dataset)[field], df)
    }
  }
  
  # Sort table with NUMERIC types first
  t<-formattable::formattable(tidyTable[order(tidyTable$SYMBOLIC),],
                              list(SYMBOLIC = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                                          x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   NUMERIC  = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                                          x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   Categories  = formatter("span",style = x ~ style(color = "black"), x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                   Min         = formatter("span",style = x ~ style(color = "black"), ~ ifelse(SYMBOLIC,"-",format(Min,  nsmall=2, big.mark=","))),
                                   Mean        = formatter("span",style = x ~ style(color = "black"), ~ ifelse(SYMBOLIC,"-",format(Mean, nsmall=2, big.mark=","))),
                                   Max         = formatter("span",style = x ~ style(color = "black"), ~ ifelse(SYMBOLIC,"-",format(Max,  nsmall=2, big.mark=","))),
                                   Skew        = formatter("span",style = x ~ style(color = "black"), ~ ifelse(SYMBOLIC,"-",sprintf("%.2f", Skew)))
                              ))
  
  print(t)
  saveWorkbook(wb, file="CategoryStatistcs.xlsx", overwrite=TRUE)
}


# ************************************************
# Nrescale() :
#
# These are the real values, that we scale between 0-1
# i.e. x-min / (max-min)
#
# INPUT:   vector - input - values to scale
#
# OUTPUT : vector - scaled values to [0.0,1.0]
# ************************************************
Nrescale <- function(input){
  
  minv <- min(input)
  maxv <- max(input)
  return((input-minv)/(maxv-minv))
}


# ************************************************
# NPREPROCESSING_discreetNumeric() :
#
# Test NUMERIC field if DISCREET or ORDINAL
#
# INPUT: data frame      - dataset     - input data
#        int             - cutoff      - Number of empty bins needed to determine discreet (1-10)
#        int             - binsize     - Number of bins in histogram
#
# OUTPUT : vector strings - Types per field {DISCREET, ORDINAL, SYMBOLIC}
# ************************************************
# Uses histogram
# Plots histogram for visulisation
# ************************************************
NPREPROCESSING_fieldTypes<-function(dataset, binsize, cutoff){
  
  fieldTypes <- vector()
  
  for(field in 1:(ncol(dataset))){
    
    # Filter for only NUMERIC fields
    if (is.numeric(dataset[,field])) {
      
      # Scale the whole field to between 0 and 1
      scaledColumn <- Nrescale(dataset[,field])
      
      # Generate the "cutoff" points of bins
      # 0-0.1, 0.1-0.2...0.9-1.0 if binsize=10
      cutpoints <- seq(0,1,length=binsize+1)
      
      # Create bin containers
      bins <- vector()
      
      # Sort values into correct bin range
      for (i in 2:(binsize+1)){
        rangeBucket <- scaledColumn[(scaledColumn<=cutpoints[i])&(scaledColumn>cutpoints[i-1])]
        bins <- append(bins,length(rangeBucket))
      }
      
      # % Value of the count (i.e. density)
      bins <- (bins/length(scaledColumn))*100.0
      
      
      # If number of bins with less than 1%, is greater than cutoff
      # then field is determined as DISCREET
      if (length(which(bins<1.0))>cutoff)
        fieldTypes[field] <- TYPE_DISCREET
      else
        fieldTypes[field] <- TYPE_ORDINAL
      
      
      graphTitle <- "Decision based on cutoff value:"
      
      # Histogram
      barplot(bins, main=paste(graphTitle, cutoff, ":", fieldTypes[field]),
              xlab=names(dataset[field]),
              names.arg = 1:binsize,bty="n")
      
    }else
    {
      fieldTypes[field] <- TYPE_SYMBOLIC
    }
  } 
  return(fieldTypes)
}






# ************************************************
# NPREPROCESSING_categorical() :
#
# Transform SYMBOLIC or DISCREET fields using 1-hot-encoding
#
# INPUT: data frame    - dataset      - symbolic fields
#        vector string - fieldTypes  - types per field {ORDINAL, SYMBOLIC, DISCREET}
#
# OUTPUT : data frame    - transformed dataset
# ************************************************
# Small number of literals only otherwise too many dimensions
# Uses 1-hot-encoding if more than 2 unique literals in the field
# Otherwise converts the 2 literals into one field of {0,1}
# ************************************************
NPREPROCESSING_categorical<-function(dataset, fieldTypes){
  
  # Dataframe of the transformed categorical fields
  # Create one column and fill with rowcount with NA
  categorical<-data.frame(first=rep(NA, nrow(dataset)), stringsAsFactors=FALSE)
  
  for(field in 1:(ncol(dataset))){
    
    # Only for SYMBOLIC or DISCREET fields (NEED TO SOLVE WHAT TO DO WITH DISCREET BUCKETTING)
    # if ((fieldTypes[field]==TYPE_SYMBOLIC)||(fieldTypes[field]==TYPE_DISCREET)) {
    if (fieldTypes[field]==TYPE_SYMBOLIC) {
      
      # Create a list of unique values in the field (each is a literal)
      literals <- as.vector(unique(dataset[,field]))
      numberLiterals <- length(literals)
      
      # If just two literals in the field, convert to 0 and 1
      if (numberLiterals==2){
        transformed <- ifelse (dataset[,field]==literals[1],0.0,1.0)
        categorical <- cbind(categorical,transformed)
        colnames(categorical)[ncol(categorical)] <- colnames(dataset)[field]
        
      } else
      {
        # 1-hot encoding FOR SMALL NUMBER of literals(set max limit)
        if (numberLiterals<=MAX_LITERALS){
          for(num in 1:numberLiterals){
            nameOfLiteral<-literals[num]
            hotEncoding<-ifelse (dataset[,field]==nameOfLiteral,1.0,0.0)
            
            # Warning - do not convert the field if their are too few literals
            # Use log of number of recrods as the measure
            literalsActive<-sum(hotEncoding==1)
            if (literalsActive>log(length(hotEncoding))) {
              categorical<-cbind(categorical,hotEncoding)
              
              # Field name has the "_" seperator to make easier to read
              colnames(categorical)[ncol(categorical)]<-paste(colnames(dataset)[field],
                                                              "_",
                                                              NPREPROCESSING_removePunctuation(nameOfLiteral),
                                                              sep="")
            }
            else {
              print(paste("Ignoring in field:",names(dataset)[field],
                          "Literal:",nameOfLiteral,
                          "Too few=",literalsActive))
            }
          }
        } else {
          stop(paste("Error - too many literals in:",names(dataset)[field], numberLiterals))
        }
        
      }
    }
  }
  
  # Remove first column full of NA due to creation of empty data frame
  return(categorical[,-1]) 
  
  # write.csv(categorical, file="1-hot-encoding-SYMBOLIC.csv")
}




NplotOutliers<-function(sorted,outliers,fieldName){
  
  plot(1:length(sorted),sorted,pch=1,xlab="Unique records",ylab=paste("Sorted values",fieldName),bty="n")
  if (length(outliers)>0)
    points(outliers,sorted[outliers],col="red",pch=19)
}



# CUTOFF_OUTLIER <- 0.99

# ************************************************
# Test if any ordinals are outliers and replace with mean values
# Null hyposis is there are no outliers
# We reject this if the p-value<significance (i.e. 0.05), confidence=95%

NPREPROCESSING_outlier<-function(ordinals,confidence){
  
  for(field in 1:(ncol(ordinals))){
    
    sorted <- unique(sort(ordinals[,field],decreasing=TRUE))
    
    outliers <- which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    
    NplotOutliers(sorted,outliers,colnames(ordinals)[field])
    
    # If found records with outlier values
    if ((length(outliers>0))){
      
      # If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence>0){
        outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
        
        # sorted<-unique(sort(outliersGone,decreasing=TRUE))
        
        # NplotOutliers(sorted,vector(),colnames(ordinals)[field])
        ordinals[,field]<-outliersGone #Put in the values with the outliers replaced by means
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers),"Replaced with MEAN"))
      } else {
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers)))
      }
    }
    
  }
  return(ordinals)
}



Nrescaleentireframe<-function(dataset){
  
  scaled<-sapply(as.data.frame(dataset),Nrescale)
  return(scaled)
}








NPREPROCESSING_redundantFields<-function(dataset,cutoff){
  
  print(paste("Before redundancy check Fields=",ncol(dataset)))
  
  #Remove any fields that have a stdev of zero (i.e. they are all the same)
  xx<-which(apply(dataset, 2, function(x) sd(x, na.rm=TRUE))==0)+1
  
  if (length(xx)>0L)
    dataset<-dataset[,-xx]
  
  #Kendall is more robust for data do not necessarily come from a bivariate normal distribution.
  cr<-cor(dataset, use="everything")
  #cr[(which(cr<0))]<-0 #Positive correlation coefficients only
  NPLOT_correlagram(cr)
  
  correlated<-which(abs(cr)>=cutoff,arr.ind = TRUE)
  list_fields_correlated<-correlated[which(correlated[,1]!=correlated[,2]),]
  
  if (length(list_fields_correlated)>0){
    
    print("Following fields are correlated")
    print(list_fields_correlated)
    
    #We have to check if one of these fields is correlated with another as cant remove both!
    v<-vector()
    numc<-nrow(list_fields_correlated)
    for (i in 1:numc){
      if (length(which(list_fields_correlated[i,1]==list_fields_correlated[i:numc,2]))==0) {
        v<-append(v,list_fields_correlated[i,1])
      }
    }
    print("Removing the following fields")
    print(names(dataset)[v])
    
    return(dataset[,-v]) #Remove the first field that is correlated with another
  }
  return(dataset)
}




NPLOT_correlagram<-function(cr){
  
  #Defines the colour range
  col<-colorRampPalette(c("green", "red"))
  
  #To fir on screen, convert field names to a numeric
  rownames(cr)<-1:length(rownames(cr))
  colnames(cr)<-rownames(cr)
  
  corrplot::corrplot(abs(cr),method="square",
                     order="FPC",
                     cl.ratio=0.2,
                     cl.align="r",
                     tl.cex = 0.6,cl.cex = 0.6,
                     cl.lim = c(0, 1),
                     mar=c(1,1,1,1),bty="n")
}








NPREPROCESSING_splitdataset<-function(combinedML){
  
  # **** Create a TRAINING dataset using HOLDOUT % of the records
  
  combinedML<-combinedML[order(runif(nrow(combinedML))),]
  training_records<-round(nrow(combinedML)*(HOLDOUT/100))
  
  train <- 1:training_records
  test <- -train
  
  training_data <- combinedML[train,]
  testing_data = combinedML[test,]
  
  retList<-list("train"=training_data,
                "test"=testing_data)
  return(retList)
}













##########################################################################


myModelling<-function(training_data,testing_data){
  
  formular<-myModelFormula(dataset=training_data,fieldNameOutput=OUTPUT_FIELD)
  
  #Build a logistic regression classifier on training dataset
  logisticModel<-stats::glm(formular,data=training_data,family=quasibinomial)
  
  # Get probabilities of being class 1 from the classifier
  probabilities<-predict(logisticModel, testing_data,type="response")
  
  #Evaluate the classifier on test dataset
  threshold<-0.7
  results<-myEvaluateClassifier(probs=probabilities,
                                testing_data=testing_data,
                                threshold=threshold)
  
  # This outputs our results into the "Viewer" in RStudio
  NprintMeasures(results)
  
  # Plot FPR/TPR through threshold range
  results<-myPerformancePlot(probs=probabilities,testing_data=testing_data)
  
  NprintMeasures(results)
  
  # Optional output of strengths
  importance<-as.data.frame(caret::varImp(logisticModel, scale = TRUE))
  row.names(importance)<-gsub("[[:punct:][:blank:]]+", "", row.names(importance))
  barplot(t(importance[order(importance$Overall),,drop=FALSE]),las=2, border = 0, cex.names =0.8)
  
  print("evaluation complete")
}




NPREPROCESSING_removePunctuation<-function(fieldName){
  return(gsub("[[:punct:][:blank:]]+", "", fieldName))
}






myModelFormula<-function(dataset,fieldNameOutput){
  inputs<-paste(names(dataset)[which(names(dataset)!=fieldNameOutput)],collapse = "+")
  output<-paste(fieldNameOutput,"~")
  formular=as.formula(paste(output,inputs))
  return(formular)
} #endof myModelFormula()





myEvaluateClassifier<-function(probs,testing_data,threshold) {
  
  predictedClass<-ifelse(probs<threshold,0,1)
  expectedClass<-testing_data[,OUTPUT_FIELD]
  
  results<-NcalcConfusion(expectedClass=expectedClass,predictedClass=predictedClass)
  
  return(results)
} #endof myEvaluateClassifier()




NcalcConfusion<-function(expectedClass,predictedClass){
  
  
  
  confusion<-table(factor(predictedClass,levels=0:1),factor(expectedClass,levels=0:1))
  
  
  
  # This "converts" the above into our preferred format
  
  
  
  TP<-as.double(confusion[2,2])
  FN<-as.double(confusion[1,2])
  FP<-as.double(confusion[2,1])
  TN<-as.double(confusion[1,1])
  
  
  
  return(NcalcMeasures(TP,FN,FP,TN))
  
  
  
} 



NprintMeasures<-function(results){
  
  
  
  #This outputs our results into the "Viewer" in RStudio
  tidyTable<-data.frame(t(t(results)))
  names(tidyTable)[1]<-"Metric"
  
  
  
  t<-formattable::formattable(tidyTable,list(
    TP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TP)),
    FN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FN)),
    TN = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",TN)),
    FP = formatter("span",style = x ~ style(color = "black"),~sprintf("%.0f",FP))))
  print(t)
}







myPerformancePlot<-function(probs,testing_data){
  
  
  
  toPlot<-data.frame()
  
  
  
  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-myEvaluateClassifier(probs=probs,testing_data=testing_data,threshold=threshold)
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$TPR))
  }
  
  
  
  # the Youden index is the vertical distance between the 45 degree line
  # and the point on the ROC curve.
  # Higher values of the Youden index are better than lower values.
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5082211/
  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1
  
  
  
  # Euclidean distance sqrt((1 − sensitivity)^2+ (1 − specificity)^2)
  # To the top left (i.e. perfect classifier)
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+((toPlot$fpr)^2))
  
  
  
  # ************************************************
  # Plot threshold graph
  
  
  
  # Sensitivity
  plot(toPlot$x,toPlot$tpr,
       type="l",lwd=3, col="blue",
       xlab="Threshold",
       ylab="%Rate",
       main="Threshold Perfomance Loan Classifier Model")
  
  
  
  # Plot the specificity (1-FPR)
  lines(toPlot$x,100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)
  
  
  
  # The point where specificity and sensitivity are the same
  crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]
  abline(v=crosspoint,col="red",lty=3,lwd=2)
  
  
  
  # Plot the Euclidean distance to "perfect" classifier (smallest the best)
  lines(toPlot$x,toPlot$distance,type="l",col="green",lwd=2,lty=3)
  mindist<-toPlot$x[which(toPlot$distance==min(toPlot$distance))]
  abline(v=mindist,col="green",lty=3,lwd=2)
  
  
  
  # Youdan (Vertical distance between the 45 degree line and the point on the ROC curve )
  lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)
  indexToBest<-which(toPlot$youdan==max(toPlot$youdan))
  maxYoudan<-toPlot$x[indexToBest]
  abline(v=maxYoudan,col="purple",lty=3,lwd=2)
  
  
  
  legend("bottom",c("TPR","1-FPR","Distance","Youdan"),col=c("blue","red","green","purple"),lty=1:2,lwd=2)
  text(x=0,y=50, adj = c(-0.2,2),cex=1,col="black",paste("THRESHOLDS:\nDistance=",mindist,"\nYoudan=",maxYoudan))
  
  
  
  # ************************************************
  # ROC graph using a library
  
  
  
  rr<-pROC::roc(response=testing_data[,OUTPUT_FIELD],
                predictor=probs,
                plot=TRUE,auc=TRUE, auc.polygon=TRUE,
                percent=TRUE, grid=TRUE,print.auc=TRUE,
                main="ROC Loan Classifier Model",
                xlab="Specificity (1-FPR) %",
                ylab="Sensitivity (TPR) %")
  
  
  
  # Selects the "best" threshold based on distance
  analysis<-coords(rr, x="best",
                   best.method="closest.topleft",
                   ret=c("threshold",
                         "specificity",
                         "sensitivity"))
  
  
  
  fpr<-round(100.0-analysis["specificity"],digits=2)
  
  
  
  #Add crosshairs to the graph
  abline(h=analysis["sensitivity"],col="red",lty=3,lwd=2)
  abline(v=analysis["specificity"],col="red",lty=3,lwd=2)
  
  
  
  #Annote with text
  annotate<-paste("Threshold: ",round(analysis["threshold"],digits=4L),
                  " TPR: ",round(analysis["sensitivity"],digits=2L),
                  "% FPR: ",fpr,"%",sep="")
  
  
  
  text(x=analysis["specificity"],
       y=analysis["sensitivity"], adj = c(-0.2,2),cex=1,
       col="red",annotate)
  
  
  
  #Use the "best" distance threshold to evaluate classifier
  results<-myEvaluateClassifier(probs=probs,
                                testing_data=testing_data,
                                threshold=analysis["threshold"])
  
  
  
  #Use the Youdan threshold to evaluate classifier
  #results<-myEvaluateClassifier(probs=probs,
  #                              testing_data=testing_data,
  #                              threshold=maxYoudan)
  
  
  
  return(results)
} #endof myPerformancePlot()




NcalcMeasures<-function(TP,FN,FP,TN){
  
  
  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=100.0*((TP+TN)/(TP+FP+FN+TN)),
                  "pgood"=   100.0*(TP/(TP+FP)),
                  "pbad"=    100.0*(TN/(FN+TN)),
                  "FPR"=     100.0*(FP/(FP+TN)),
                  "TPR"=     100.0*(TP/(TP+FN)),
                  "TNR"=     100.0*(TN/(FP+TN)),
                  "MCC"=     ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  )
  return(retList)
}

########################################################################################
















# ************************************************
# NConvertClass() :
#
# In original dataset, $Status is the classification label
# We need to convert this to give the minority class a value of 0
# this just makes it easiert to define the confusioon matrix!
# for the UCI-G this is a class of {0,1} being {bad, good}
#
# INPUT   :
#             Data Frame        - dataset
#
# OUTPUT  :
#             Data Frame        - dataset
#
# ************************************************
NConvertClass<-function(dataset){
  positionClassOutput<-which(names(dataset)==OUTPUT_FIELD)
  classes<-sort(table(dataset[,positionClassOutput])) #smallest class will be first
  minority<-names(classes[1])
  indexToStatus2<-which(dataset[positionClassOutput]==minority)
  dataset[positionClassOutput][indexToStatus2,]<-0
  dataset[positionClassOutput][-indexToStatus2,]<-1
  return(dataset)
}







# ************************************************
# simpleDT() :
#
# Create C5 Decision Tree on the raw dataset
# A decision tree may not need the dataset to be pre-processed
#
# INPUT   :
#             Data Frame     - train       - original train dataset
#             Data Frame     - test        - original test dataset
#             boolean        - plot        - TRUE = plot charts
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
simpleDT<-function(train,test,plot=TRUE){
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  tree<-C50::C5.0(x=train[-positionClassOutput],
                  y=factor(train[,positionClassOutput]),
                  rules=TRUE,
                  trials=1)
  
  measures<-getTreeClassifications(myTree = tree,
                                   testDataset = test,
                                   title="Original Dataset. DT C5.0")
  
  #281019NRT addfed this Function to output the tree as rules to a file
  if (plot==TRUE){
    
    # Get importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Strength"
    
    importance<-importance[order(importance$Strength,decreasing=TRUE),,drop=FALSE]
    
    print(formattable::formattable(importance))
    
    # Plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.7,
            main="Basic C5.0")
    
    dftreerules<-NDT5RuleOutput(tree)
    print(formattable::formattable(dftreerules))
  }
  
  return(measures)
} #endof simpleDT()



# ************************************************
# getTreeClassifications() :
#
# Put in test dataset and get out class predictions of the decision tree
# Determine the threshold, plot the results and calculate metrics
#
# INPUT   :   object         - myTree        - tree
#         :   Data Frame     - testDataset - dataset to evaluate
#         :   string         - title        - string to plot as the chart title
#         :   int            - classLabel   - lable given to the positive (TRUE) class
#         :   boolean        - plot         - TRUE to output results/charts
#
# OUTPUT  :   List       - Named evaluation measures
#
# ************************************************
getTreeClassifications<-function(myTree,
                                 testDataset,
                                 title,
                                 classLabel=1,
                                 plot=TRUE){
  
  positionClassOutput=which(names(testDataset)==OUTPUT_FIELD)
  
  #test data: dataframe with with just input fields
  test_inputs<-testDataset[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
  
  testPredictedClassProbs<-predict(myTree,test_inputs, type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs))==classLabel)
  
  # Get the probabilities for classifying the good loans
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predictedProbs,
                                plot=plot,
                                title=title)
  
  if (plot==TRUE)
    NprintMeasures(results=measures)
  
  return(measures)
  
} #endof getTreeClassifications()















# ************************************************
# NdetermineThreshold() :
#
# For the range of threholds [0,1] calculate a confusion matrix
# and classifier metrics.
# Deterime "best" threshold based on either distance or Youdan
# Plot threshold chart and ROC chart
#
# Plot the results
#
# INPUT   :   vector double  - probs        - probability of being class 1
#         :   Data Frame     - testing_data - dataset to evaluate
#         :   boolean        - plot         - TRUE=create charts otherwise don't
#         :   string         - title        - string to plot as the chart title
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# Uses   library(pROC)
# 241019NRT - added plot flag and title for charts
# ************************************************
NdetermineThreshold<-function(test_predicted,
                              test_expected,
                              plot=TRUE,
                              title=""){
  
  # Helper local scope function
  getFirst<-function(values){
    if (length(values)>1){
      return(values[1])
    } else
      return(values)
  }
  
  toPlot<-data.frame()
  
  #Vary the threshold
  for(threshold in seq(0,1,by=0.01)){
    results<-NEvaluateClassifier(test_predicted=test_predicted,
                                 test_expected=test_expected,
                                 threshold=threshold)
    toPlot<-rbind(toPlot,data.frame(x=threshold,fpr=results$FPR,tpr=results$TPR))
  }
  
  # the Youden index is the vertical distance between the 45 degree line
  # and the point on the ROC curve.
  # Higher values of the Youden index are better than lower values.
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5082211/
  toPlot$youdan<-toPlot$tpr+(1-toPlot$fpr)-1
  indexToBest<-getFirst(which(toPlot$youdan==max(toPlot$youdan)))
  maxYoudan<-toPlot$x[indexToBest]
  
  # Euclidean distance sqrt((1 − sensitivity)^2+ (1 − specificity)^2)
  # To the top left (i.e. perfect classifier)
  toPlot$distance<-sqrt(((100-toPlot$tpr)^2)+((toPlot$fpr)^2))
  
  #241019 select just the first min distance, as might be more
  mindist<-getFirst(toPlot$x[which(toPlot$distance==min(toPlot$distance))])
  
  # ************************************************
  # Plot threshold graph
  
  if (plot==TRUE){
    # Sensitivity (TPR)
    plot(toPlot$x,toPlot$tpr,
         xlim=c(0, 1), ylim=c(0, 100),
         type="l",lwd=3, col="blue",
         xlab="Threshold",
         ylab="%Rate",
         main=paste("Threshold Perfomance Classifier Model",title))
    
    # Plot the specificity (1-FPR)
    lines(toPlot$x,100-toPlot$fpr,type="l",col="red",lwd=3,lty=1)
    
    # The point where specificity and sensitivity are the same
    crosspoint<-toPlot$x[which(toPlot$tpr<(100-toPlot$fpr))[1]]
    
    if (!is.na(crosspoint)){
      if (crosspoint<1)
        abline(v=crosspoint,col="red",lty=3,lwd=2)
    }
    
    # Plot the Euclidean distance to "perfect" classifier (smallest the best)
    lines(toPlot$x,toPlot$distance,type="l",col="green",lwd=2,lty=3)
    
    # Plot the min distance, as might be more
    abline(v=mindist,col="green",lty=3,lwd=2)
    
    # Youdan (Vertical distance between the 45 degree line and the point on the ROC curve )
    lines(toPlot$x,toPlot$youdan,type="l",col="purple",lwd=2,lty=3)
    
    abline(v=maxYoudan,col="purple",lty=3,lwd=2)
    
    legend("bottom",c("TPR","1-FPR","Distance","Youdan"),
           col=c("blue","red","green","purple"),
           lty=c(1,1,3,3),
           lwd=2)
    
    text(x=0,y=50, adj = c(-0.2,2),cex=1,
         col="black",
         paste("THRESHOLDS:\nDistance=",mindist,"\nYoudan=",maxYoudan))
    
    # ************************************************
    # ROC graph using a library
    
    rr<-pROC::roc(response=test_expected,
                  predictor=test_predicted,
                  plot=TRUE,
                  auc=TRUE,
                  auc.polygon=TRUE,
                  percent=TRUE,
                  grid=TRUE,
                  print.auc=TRUE,
                  main=paste("ROC for Classifier Model",title),
                  xlab="Specificity (1-FPR) %",
                  ylab="Sensitivity (TPR) %")
    
    # Selects the "best" threshold based on distance
    analysis<-coords(rr, x="best",transpose = FALSE,
                     best.method="closest.topleft",
                     ret=c("threshold",
                           "specificity",
                           "sensitivity"))
    
    fpr<-round(100.0-analysis["specificity"],digits=2)
    
    #Add crosshairs to the graph
    abline(h=analysis["sensitivity"],col="red",lty=3,lwd=2)
    abline(v=analysis["specificity"],col="red",lty=3,lwd=2)
    
    #Annote with text
    annotate<-paste("Threshold: ",round(analysis["threshold"],digits=4L),
                    " TPR: ",round(analysis["sensitivity"],digits=2L),
                    "% FPR: ",fpr,"%",sep="")
    
    text(x=analysis["specificity"],
         y=analysis["sensitivity"], adj = c(-0.2,2),cex=1,
         col="red",annotate)
    
  } # endof if plotting
  
  # Select the threshold - I have choosen distance
  
  myThreshold<-mindist      # Min Distance should be the same as analysis["threshold"]
  
  #Use the "best" distance threshold to evaluate classifier
  results<-NEvaluateClassifier(test_predicted=test_predicted,
                               test_expected=test_expected,
                               threshold=myThreshold)
  
  results$threshold<-myThreshold
  
  return(results)
} #endof myPerformancePlot()

# ************************************************
# NprintDTRules() :
#
# INPUT: text - filename
#
# OUTPUT : Frame - dataset
# ************************************************
NprintDTRules<-function(dtrules, filename){
  
  sink(filename)
  
  for (r in 1:nrow(dtrules)){
    print(paste("[",r,"]",dtrules$Rule[r],"==",(ifelse(dtrules$Class[r]==0,"BAD","GOOD"))))
  }
  sink()
}
# ************************************************
# DECISION TREE CONVERT DT RULES TO ASCII FORMATTED RULES
#
# <anticedent 1> AND <anticedent 2> ...
# Each anticedent is: [field][comparision][value]
#
# INPUT: Object - tree - Trained tree
#
# OUTPUT: data frame of rules, class and anticedents
# ************************************************
NDT5RuleOutput<-function(tree){
  #library(stringr)
  x<-summary(tree)[1]
  x<-substr(x,regexpr("Rules:",x)[1]+8,nchar(x))
  x<-substr(x,1,regexpr("Evaluation on training data",x)[1]-1)
  x<-gsub("[\n\t]", "*", x)
  df_of_rules<-data.frame(matrix(ncol=3,nrow=tree$size),stringsAsFactors = FALSE)
  df_of_rules<-setNames(df_of_rules,c("Rule","Class","Anti"))
  
  numberofrules<-tree$size
  # 271019 allow for multiple trees (i.e. boosted)
  if (length(numberofrules)>1){
    numberofrules<-numberofrules[1]
    warning("Prof Nick says: More than one tree found. Extracting rules for just the first")
  }
  
  totalAnticedents<-0
  for (ruleNumber in 1:numberofrules){
    start<-regexpr("\\*\\*",x)[1]+2
    end<-regexpr("->",x)[1]-3
    onerule<-substr(x,start,end) #Single rule, anticedents seperated by '**'
    onerule<-gsub("\\*\\*"," AND ",onerule) #Rule now has "AND" between anticedents
    #onerule<-convertNormalisedDTRuleToRealWorld(onerule)
    NumAnticedents<-str_count(onerule,"AND")+1
    totalAnticedents=totalAnticedents+NumAnticedents
    classpos<-regexpr("class ",x)+6
    classID<-as.numeric(substr(x,classpos,classpos))  #This has the class of the rule, i.e. {0,1}
    df_of_rules$Rule[ruleNumber]<-onerule
    df_of_rules$Class[ruleNumber]<-ifelse(classID==0,"BAD","GOOD") # Convert class to label
    df_of_rules$Anti[ruleNumber]<-NumAnticedents
    x<-substr(x,classpos,nchar(x))
    st<-regexpr("\\*\\*",x)[1]+2 #move past the rule ID
    x<-substr(x,st,nchar(x))
  }
  return(df_of_rules)
}



# ************************************************
# NEvaluateClassifier() :
#
# Use dataset to generate predictions from model
# Evaluate as classifier using threshold value
#
# INPUT   :   vector double     - probs        - probability of being class 1
#             Data Frame        - testing_data - Dataset to evaluate
#             double            - threshold     -cutoff (probability) for classification
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# ************************************************
NEvaluateClassifier<-function(test_predicted,test_expected,threshold) {
  
  predictedClass<-ifelse(test_predicted<threshold,0,1)
  
  results<-NcalcConfusion(expectedClass=test_expected,
                          predictedClass=predictedClass)
  
  return(results)
} #endof NEvaluateClassifier()













######################################################################


# ************************************************
# randomForest() :
#
# Create Random Forest on pre-processed dataset
#
# INPUT   :
#         :   Data Frame     - train       - train dataset
#             Data Frame     - test        - test dataset
#             boolean        - plot        - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
randomForest<-function(train,test,plot=TRUE){
  
  myTitle<-(paste("Preprocessed Dataset. Random Forest=",FOREST_SIZE,"trees"))
  print(myTitle)
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  rf<-randomForest::randomForest(train_inputs,
                                 factor(train_expected),
                                 ntree=FOREST_SIZE ,
                                 importance=TRUE,
                                 mtry=sqrt(ncol(train_inputs)))
  
  
  # ************************************************
  # Use the created decision tree with the test dataset
  measures<-getTreeClassifications(myTree = rf,
                                   testDataset = test,
                                   title=myTitle,
                                   plot=plot)
  
  if (plot==TRUE){
    # Get importance of the input fields
    importance<-randomForest::importance(rf,scale=TRUE,type=1)
    importance<-importance[order(importance,decreasing=TRUE),,drop=FALSE]
    
    colnames(importance)<-"Strength"
    
    barplot(t(importance),las=2, border = 0,
            cex.names =0.7,
            main=myTitle)
    
    print(formattable::formattable(data.frame(importance)))
  }
  
  return(measures)
} #endof randomForest()

























#######################################################################################


# ************************************************
# mlpNeural() :
#
# SHALLOW BASIC MLP TRAINING
# Uses Keras or h2o library to create a basic 3 layer MLP
#
# INPUT   :
#         :   Data Frame     - rawDataset  - original dataset
#             boolean        - plot        - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# 311019NRTY Updated to use either h2o or Keras library
# ************************************************
mlpNeural<-function(train,test, plot=TRUE){
  
  myTitle<-paste("Preprocessed Dataset. MLP. Hidden=",BASICNN_HIDDEN,sep="")
  print(myTitle)
  
  # Set to TRUE to use the h2o library
  # otheriwse FALSE to try to use the Keras library
  
  if (TRUE) {
    N_DEEP_Initialise()
    
    mlp_classifier<-N_DEEP_TrainClassifier(train=train,
                                           fieldNameOutput=OUTPUT_FIELD,
                                           hidden=BASICNN_HIDDEN,
                                           stopping_rounds=DEEP_STOPPING,
                                           stopping_tolerance=DEEP_TOLERANCE,
                                           activation=DEEP_ACTIVATION,
                                           reproducible=DEEP_REPRODUCABLE)
    
    plot(mlp_classifier,metric="classification_error")
    
    # Evaluate the deep NN as we have done previously
    measures<-N_EVALUATE_DeepNeural(test=test,
                                    fieldNameOutput=OUTPUT_FIELD,
                                    deep=mlp_classifier,
                                    plot=plot,
                                    myTitle = myTitle)
  } else {
    
    mlp_classifier<-N_MLP_TrainClassifier(train=train,
                                          fieldNameOutput=OUTPUT_FIELD,
                                          hidden=BASICNN_HIDDEN,
                                          plot=plot)
    
    measures<-N_evaluate_MLP(test=test,
                             fieldNameOutput=OUTPUT_FIELD,
                             mlp_classifier=mlp_classifier,
                             plot=plot,
                             myTitle=myTitle)
  } #endof if()
  
  return(measures)
} #endof mlpNeural()







# ************************************************
# N_DEEP_Initialise()
# Initialise the H2O server
#
# INPUT:
#         Bool       - reproducible       - TRUE if model must be reproducable each run
#
# OUTPUT : none
# ************************************************
N_DEEP_Initialise<-function(reproducible=FALSE){
  
  library(h2o)
  
  print("Initialise the H2O server")
  #Initialise the external h20 deep learning local server if needed
  #130517NRT - set nthreads to -1 to use maximum so fast, but set to 1 to get reproducable results
  #080819NRT - use reproducable parameter
  if (reproducible==TRUE)
    nthreads<-1
  else
    nthreads<- -1
  
  h2o.init(max_mem_size = "5g",nthreads = nthreads)
  
  h2o.removeAll() # 261019NRT clean slate - just in case the cluster was already running
  #h2o.no_progress()
}







# ************************************************
# N_EVALUATE_DeepNeural() :
#
# Evaluate Deep Neural Network classifier
# Generates probabilities from the classifier
#
# INPUT: Data Frame    -  test             - scaled [0.0,1.0], fields & rows
#        String        -  fieldNameOutput  - Name of the field that we are training on (i.e.Status)
#        Object         - deep             - trained NN including the learn weights, etc.
#         boolean      - plot              - TRUE = output charts/results
#         string       - myTitle           - title on results
#
# OUTPUT :
#         list - metrics from confusion matrix
# ************************************************
# Uses   library(h2o)

N_EVALUATE_DeepNeural<-function(test,fieldNameOutput, deep,plot,myTitle){
  
  #Creates the h2o test dataset
  test[fieldNameOutput] <- lapply(test[fieldNameOutput] , factor) #Output class has to be a R "factor"
  test_h2o <- as.h2o(test, destination_frame = "testdata")
  
  pred <- h2o::h2o.predict(deep, test_h2o)
  
  test_predicted<-as.vector(pred$p1)  #Returns the probabilities of class 1
  
  positionClassOutput<-which(names(test)==fieldNameOutput)
  
  # train data: vector with the expedcted output
  test_expected<-test[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=test_predicted,
                                plot=plot,
                                title=myTitle)
  
  if (plot==TRUE)
    NprintMeasures(results=measures)
  
  return(measures)
}

# ************************************************
# N_MLP_TrainClassifier()
#
# MLP NEURAL NETWORK
#
# INPUT:  Frame      - train              - scaled [0.0,1.0], fields & rows
#         String     - fieldNameOutput    - Name of the field to classify
#         Int Vector - hidden             - Number of hidden layer neurons for each layer
#         boolean    - plot               - TRUE = output charts/results
#
# OUTPUT: object     - trained neural network
# ************************************************
N_MLP_TrainClassifier<- function(train,
                                 fieldNameOutput,
                                 hidden,
                                 plot
){
  
  positionClassOutput<-which(names(train)==fieldNameOutput)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  x<-as.matrix(train_inputs)
  y<-keras::to_categorical(train_expected,num_classes = 2)
  
  mlp_classifier = keras_model_sequential()
  
  # add layers, first layer needs input dimension
  mlp_classifier %>%
    keras::layer_dense(input_shape = ncol(x), units=ncol(x), activation = "relu") %>%
    keras::layer_dropout(0.2) %>%
    keras::layer_dense(units = hidden, activation = "relu") %>%
    keras::layer_dropout(0.2) %>%
    keras::layer_dense(units = 2, activation = "softmax")
  
  # add a loss function and optimizer
  mlp_classifier %>%
    keras::compile(
      loss = "categorical_crossentropy",
      optimizer = "adagrad",
      metrics = "accuracy"
    )
  
  # train model with our training data set
  fit = mlp_classifier %>%
    keras::fit(
      x = x,
      y = y,
      shuffle = T,
      batch_size = 5,
      validation_split = 0.2,
      epochs = BASICNN_EPOCHS,
      callbacks = c(
        callback_early_stopping(monitor = "val_loss", patience = 8, mode = "auto")),
      verbose=0, view_metrics=0
    )
  
  # Plot the neural network error (loss) udring training
  if (plot==TRUE)
    print(plot(fit))
  
  return(mlp_classifier)
}

# ************************************************
# N_EVALUATE_MLP() :
#
# Evaluate MLP Neural Network classifier
# Generates probabilities from the classifier
#
# INPUT: Data Frame    -  testing_data     - scaled [0.0,1.0], fields & rows
#        String        -  fieldNameOutput  - Name of the field that we are training on (i.e.Status)
#        Object        - mlp_classifier    - trained NN including the learn weights, etc.
#         boolean      - plot              - TRUE = output charts/results
#         string       - myTitle           - title on results
#
# OUTPUT :
#         list - metrics from confusion matrix
# ************************************************

N_evaluate_MLP<-function(test,fieldNameOutput,mlp_classifier,plot,myTitle){
  
  positionClassOutput<-which(names(test)==fieldNameOutput)
  
  # test data: dataframe with with just input fields
  test_inputs<-test[-positionClassOutput]
  
  # Generate class membership probabilities
  # Column 1 is for class 0 (bad loan) and column 2 is for class 1 (good loan)
  
  testPredictedAllClassProbs<-predict(mlp_classifier,as.matrix(test_inputs))
  
  # Probabilities for just class 1
  testPredictedClassProbs<-testPredictedAllClassProbs[,2]
  
  # train data: vector with the expedcted output
  test_expected<-test[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_expected=test_expected,
                                test_predicted=testPredictedClassProbs,
                                plot=plot,
                                title=myTitle)
  if (plot==TRUE)
    NprintMeasures(results=measures)
  
  return(measures)
}







# ************************************************
# N_DEEP_TrainClassifier()
#
# h2O NEURAL NETWORK : DEEP LEARNING CLASSIFIER TRAIN
#
# INPUT:  Frame      - train              - scaled [0.0,1.0], fields & rows
#         String     - fieldNameOutput    - Name of the field to classify
#         Int Vector - hidden             - Number of hidden layer neurons for each layer
#         int        - stopping_rounds    - Number of times no improvement before stop
#         double     - stopping_tolerance - Error threshold
#         String     - activation         - Name of activation function
#         Bool       - reproducible       - TRUE if model must be reproducable each run
#
# OUTPUT: object     - trained neural network
# ************************************************
N_DEEP_TrainClassifier<- function(train,
                                  fieldNameOutput,
                                  hidden,
                                  stopping_rounds,
                                  stopping_tolerance,
                                  activation,
                                  reproducible){
  
  positionOutput<-which(names(train)==fieldNameOutput)
  
  #Creates the h2o training dataset
  train[fieldNameOutput] <- lapply(train[fieldNameOutput] , factor) #Output class has to be a R "factor"
  
  train_h2o <- as.h2o(train, destination_frame = "traindata")
  
  # Create validation dataset for early stopping
  splits <- h2o.splitFrame(train_h2o, 0.9, seed=1234)
  nntrain  <- h2o.assign(splits[[1]], "nntrain.hex") # 90%
  nnvalid  <- h2o.assign(splits[[2]], "nnvalid.hex") # 10%
  
  #This lists all the input field names ignoring the fieldNameOutput
  predictors <- setdiff(names(train_h2o), fieldNameOutput)
  
  # Deep training neural network
  # Updated 13/5/17 - set reproducible = TRUE so that the same random numbers are used to initalise
  # 281019NRT - added validation dataset for early stopping
  
  deep<-h2o::h2o.deeplearning(x=predictors,
                              y=fieldNameOutput,
                              training_frame = nntrain,
                              validation_frame=nnvalid,
                              epochs=BASICNN_EPOCHS,
                              hidden=hidden,
                              adaptive_rate=TRUE,
                              stopping_rounds=stopping_rounds,
                              stopping_tolerance=stopping_tolerance,
                              stopping_metric = "misclassification",
                              fast_mode=FALSE,
                              activation=activation,
                              seed=1234,
                              l1 = 1e-2,
                              l2 = 1e-2,
                              variable_importances = TRUE,
                              reproducible = TRUE)
  return(deep)
}










################################################################################








# ************************************************
# deepNeural() :
#
# DEEP LEARNING EXAMPLE USING H2O library
#
# INPUT   :
#         :   Data Frame     - rawDataset  - original dataset
#             boolean        - plot        - TRUE = output charts/results
#
# OUTPUT  :
#         :   Data Frame     - measures  - performance metrics
#
# ************************************************
deepNeural<-function(train,test,plot=TRUE){
  
  myTitle<-"Preprocessed Dataset. Deep NN"
  
  N_DEEP_Initialise()
  
  deep_classifier<-N_DEEP_TrainClassifier(train=train,
                                          fieldNameOutput=OUTPUT_FIELD,
                                          hidden=DEEP_HIDDEN,
                                          stopping_rounds=DEEP_STOPPING,
                                          stopping_tolerance=DEEP_TOLERANCE,
                                          activation=DEEP_ACTIVATION,
                                          reproducible=DEEP_REPRODUCABLE)
  
  # Evaluate the deep NN as we have done previously
  measures<-N_EVALUATE_DeepNeural(test=test,
                                  fieldNameOutput=OUTPUT_FIELD,
                                  deep=deep_classifier,
                                  plot=plot,
                                  myTitle = myTitle)
  
  if (plot==TRUE){
    # ************************************************
    # TELL ME SOMETHING INTERESTING...
    summary(deep_classifier)
    plot(deep_classifier)  # plots the scoring history
    
    # variable importance from the deep neural network
    importance = as.data.frame(h2o::h2o.varimp(deep_classifier))
    
    row.names(importance)<-importance$variable
    importanceScaled<-subset(importance, select=scaled_importance)*100
    colnames(importanceScaled)<-"Strength"
    
    barplot(t(importanceScaled),las=2, border = 0,
            cex.names =0.7,
            main=myTitle)
    
    print(formattable::formattable(data.frame(importanceScaled)))
  }
  return(measures)
} #endof deepNeural()








