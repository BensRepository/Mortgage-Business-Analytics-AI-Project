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

NPREPROCESSING_outlier<-function(ordinals,confidence){
  
  #For every ordinal field in our dataset
  for(field in 1:(ncol(ordinals))){
    
    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    NplotOutliers(sorted,outliers,colnames(ordinals)[field])
    
    #If found records with outlier values
    if ((length(outliers>0))){
      
      #070819NRT If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence>0){
        outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
        sorted<-unique(sort(outliersGone,decreasing=TRUE))
        #NplotOutliers(sorted,vector(),colnames(ordinals)[field])
        ordinals[,field]<-outliersGone #Put in the values with the outliers replaced by means
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers),"Replaced with MEAN"))
      } else {
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers)))
      }
    }
    
  }
  return(ordinals)
}

NplotOutliers<-function(sorted,outliers,fieldName){
  
  plot(1:length(sorted),sorted,pch=1,xlab="Unique records",ylab=paste("Sorted values",fieldName),bty="n")
  if (length(outliers)>0)
    points(outliers,sorted[outliers],col="red",pch=19)
}

Nrescaleentireframe<-function(dataset){
  
  scaled<-sapply(as.data.frame(dataset),Nrescale)
  return(scaled)
}

myModelFormula<-function(dataset,fieldNameOutput){
  inputs<-paste(names(dataset)[which(names(dataset)!=fieldNameOutput)],collapse = "+")
  output<-paste(fieldNameOutput,"~")
  formular=as.formula(paste(output,inputs))
  return(formular)
} #endof myModelFormula()

# ************************************************
# myEvaluateClassifier() :
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
myEvaluateClassifier<-function(probs,testing_data,threshold) {
  
  predictedClass<-ifelse(probs<threshold,0,1)
  expectedClass<-testing_data[,OUTPUT_FIELD]
  
  results<-NcalcConfusion(expectedClass=expectedClass,predictedClass=predictedClass)
  
  return(results)
} #endof myEvaluateClassifier()


# ************************************************
# myPerformancePlot() :
#
# Use dataset to generate predictions from model
# as classifier at range of thresholds values
# Plot the results
#
# INPUT   :   vector double  - probs        - probability of being class 1
#         :   Data Frame     - testing_data - dataset to evaluate
#
# OUTPUT  :   List       - Named evaluation measures
#                        - Predicted class probability
#
# Uses   library(pROC)
# ************************************************
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

NPREPROCESSING_dataset<-function(dataset, scaleFlag=FALSE){
  
  NPREPROCESSING_prettyDataset(dataset)
  
  # ************************************************
  # Determine initial field types: NUMERIC or SYMBOLIC
  fieldTypes<-NPREPROCESSING_initialFieldType(dataset)
  
  numeric_fields<-names(dataset)[fieldTypes=="NUMERIC"]
  print(numeric_fields)
  
  symbolic_fields<-names(dataset)[fieldTypes=="SYMBOLIC"]
  print(symbolic_fields)
  
  # ************************************************
  # Determine if the numeric fields might be discreet numeric
  # If there are over 3 bins with less than 1% of the values, then the field is
  # marked as a discreet numeric
  fieldTypes<-NPREPROCESSING_discreetNumeric(dataset=dataset,
                                              fieldTypes=fieldTypes,
                                              cutoff=CUTOFF_DISCREET)
  
  # ************************************************
  # IF ORDINAL TYPES:
  
  # This is a sub-set frame of just the ordinal fields
  # 271019 added drop parameters in case just one
  ordinals<-dataset[,which(fieldTypes=="ORDINAL"),drop=FALSE]
  
  # ************************************************
  # Test if any ordinals are outliers and replace with mean values
  # Null hyposis is there are no outliers
  # We reject this if the p-value<significance (i.e. 0.05), confidence=95%
  ordinals<-NPREPROCESSING_outlier(ordinals=ordinals,confidence=CUTOFF_OUTLIER)
  
  if (scaleFlag==TRUE){
    # ************************************************
    # Now z-scale
    zscaled<-apply(ordinals, MARGIN = 2,
                   FUN = function(X) (scale(X,center=TRUE,
                                            scale=TRUE)))
    
    # ************************************************
    # Scale in this case to be [0.0,1.0]
    ordinalReadyforML<-Nrescaleentireframe(as.data.frame(zscaled))
    
  } else
  {
    ordinalReadyforML<-ordinals
  }
  # We now have a frame called ordinalReadyforML of
  # just the numeric fields, nice and ready for the ML
  
  # ************************************************
  # IF SYMBOLIC TYPES:
  # This function undertakes 1-hot-encoding
  
  catagoricalReadyforML<-NPREPROCESSING_categorical(dataset = dataset,
                                                    fieldTypes=fieldTypes)
  
  # ************************************************
  # Are any of the fields of both the numeric and symbolic pre-processed datasets redundant?
  
  #Combine the two sets of data that are read for ML
  combinedML<-cbind(ordinalReadyforML,catagoricalReadyforML)
  
  # Are any of the fields redundant?
  combinedML<-NPREPROCESSING_redundantFields(dataset=combinedML,cutoff=CUTOFF_REDUNDANT)
  
  #The dataset for ML information
  print(paste("Fields=",ncol(combinedML)))
  
  # ************************************************
  # If teh names of the fields contain spaces then they
  # "confuse" some of the library algorithms
  # This removes the spaces.
  names(combinedML)<-gsub(" ", "", names(combinedML), fixed = TRUE)
  
  # ************************************************
  # Returns the pre-processed dataset
  return(combinedML)
}

NPREPROCESSING_prettyDataset<-function(dataset,...){
  
  params <- list(...)
  
  tidyTable<-data.frame(Field=names(dataset),
                        Catagorical=FALSE,
                        Symbols=0,
                        Name=0,
                        Min=0.0,
                        Mean=0.0,
                        Max=0.0,
                        Skew=0.0,
                        stringsAsFactors = FALSE)
  
  if (length(params)>0){
    names(tidyTable)[1]<-params[1]
  }
  
  for (i in 1:ncol(dataset)){
    isFieldAfactor<-!is.numeric(dataset[,i])
    tidyTable$Catagorical[i]<-isFieldAfactor
    if (isFieldAfactor){
      tidyTable$Symbols[i]<-length(unique(dataset[,i]))  #Number of symbols in catagorical
      #Gets the count of each unique symbol
      symbolTable<-sapply(unique(dataset[,i]),function(x) length(which(dataset[,i]==x)))
      majoritySymbolPC<-round((sort(symbolTable,decreasing = TRUE)[1]/nrow(dataset))*100,digits=0)
      tidyTable$Name[i]<-paste(names(majoritySymbolPC),"(",majoritySymbolPC,"%)",sep="")
    } else
    {
      tidyTable$Max[i]<-round(max(dataset[,i]),2)
      tidyTable$Mean[i]<-round(mean(dataset[,i]),2)
      tidyTable$Min[i]<-round(min(dataset[,i]),2)
      tidyTable$Skew[i]<-round(PerformanceAnalytics::skewness(dataset[,i],method="moment"),2)
    }
  }
  
  #Sort table so that all numerics are first
  t<-formattable::formattable(tidyTable[order(tidyTable$Catagorical),],
                              list(Catagorical = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   Symbols = formatter("span",style = x ~ style(color = "black"),x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                   Min = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Min, nsmall=2, big.mark=","))),
                                   Mean = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",format(Mean, nsmall=2, big.mark=","))),
                                   Max = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Max, nsmall=2, big.mark=","))),
                                   Skew = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",sprintf("%.2f", Skew)))
                              ))
  print(t)
}



fullDT<-function(train,test,boost=1,plot=TRUE){
  
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  
  # ************************************************
  # Create a standard Decision Tree using the C5.0 algorithm
  # Uses library C50
  # Outputs the tree in the format of rules
  
  myTitle<-"Preprocessed Dataset. DT C5.0"
  if (boost>1)
    myTitle<-paste(myTitle,"BOOSTED=",boost)
  
  print(myTitle)
  
  tree<-C50::C5.0(x=train_inputs,
                  factor(train_expected),
                  rules=TRUE,
                  trials=boost)
  
  # Use the created decision tree with the test dataset
  # to determine best classification threshold & calculate metrics
  measures<-getTreeClassifications(myTree = tree,
                                   testDataset = test,
                                   title=myTitle,
                                   plot=plot)
  
  if (plot==TRUE){
    
    print(summary(tree))
    
    # Get importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Strength"
    
    importance<-importance[order(importance$Strength,decreasing=TRUE),,drop=FALSE]
    
    print(formattable::formattable(importance))
    
    # Plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.7,
            main=myTitle)
    
    #Function to output the tree as rules to a file
    dftreerules<-NDT5RuleOutput(tree)
    print(formattable::formattable(dftreerules))
    
    # ************************************************
    # Creates the same  C5.0 decision tree & output as a tree structure, plot it
    # The "partykit" library requires the variables (wrongly) to be global
    print("Plot decision tree to file called tree.pdf")
    
    Global_train_inputs<<-train_inputs
    Global_train_expected<<-train_expected
    
    # :: is used to specify a function within the named package to avoid confusion
    tree<-C50::C5.0(x=Global_train_inputs,
                    factor(Global_train_expected),
                    trials=boost)
    
    # ::: is used to directly access a member of a package that is internal
    graphtree<-C50:::as.party.C5.0(tree)
    
    # The plot is large - so print to a big PDF file
    pdf(PDF_FILENAME, width=100, height=50, paper="special", onefile=F)
    
    # The number is the node level of the tree to print
    plot(graphtree[NODE_LEVEL])
    
    #This closes the PDF file
    dev.off()
  }
  return(measures)
} #endof fullDT()



# ************************************************
# myModelling() :
# Create a logistic regression classifier and evaluate
#
# INPUT       :   None
#
# OUTPUT      :   None
#
# ************************************************
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
NPREPROCESSING_setInitialFieldType<-function(name,type){
  
  #Sets in the global environment
  manualTypes<<-rbind(manualTypes,data.frame(name=name,type=type,stringsAsFactors = FALSE))
}

NPREPROCESSING_discreetNumeric<-function(dataset,fieldTypes,cutoff){
  
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){
    
    #Only for fields that are all numeric
    if (fieldTypes[field]==TYPE_NUMERIC) {
      
      #Scale the whole field (column) to between 0 and 1
      scaled_column<-Nrescale(dataset[,field])
      
      #Generate the "cutoff" points for each of 10 bins
      #so we will get 0-0.1, 0.1-0.2...0.9-1.0
      cutpoints<-seq(0,1,length=11)
      
      #This creates an empty vector that will hold the counts of ther numbers in the bin range
      bins<-vector()
      
      #Now we count how many numbers fall within the range
      #length(...) is used to count the numbers that fall within the conditional
      for (i in 2:11){
        bins<-append(bins,length(scaled_column[(scaled_column<=cutpoints[i])&(scaled_column>cutpoints[i-1])]))
      }
      
      # the 10 bins will have a % value of the count (i.e. density)
      bins<-(bins/length(scaled_column))*100.0
      
      graphTitle<-"AUTO:"
      
      #If the number of bins with less than 1% of the values is greater than the cutoff
      #then the field is deterimed to be a discreet value
      
      if (length(which(bins<1.0))>cutoff)
        fieldTypes[field]<-TYPE_DISCREET
      else
        fieldTypes[field]<-TYPE_ORDINAL
      
      #Bar chart helps visulisation. Type of field is the chart name
      barplot(bins, main=paste(graphTitle,fieldTypes[field]),
              xlab=names(dataset[field]),
              names.arg = 1:10,bty="n")
      
    } #endif numeric types
  } #endof for
  return(fieldTypes)
}

NPREPROCESSING_initialFieldType<-function(dataset){
  
  fieldTypes<-vector()
  for(field in 1:(ncol(dataset))){
    
    entry<-which(manualTypes$name==names(dataset)[field])
    if (length(entry)>0){
      fieldTypes[field]<-manualTypes$type[entry]
      next
    }
    
    if (is.numeric(dataset[,field])) {
      fieldTypes[field]<-TYPE_NUMERIC
    }
    else {
      fieldTypes[field]<-TYPE_SYMBOLIC
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
                                                              nameOfLiteral,
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
  
  # write.csv(hello, file="1-hot-encoding-SYMBOLIC.csv")
}




