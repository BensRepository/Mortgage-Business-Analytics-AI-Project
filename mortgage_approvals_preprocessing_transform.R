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

# Global Environment variables

OUTLIER_CONFIDENCE <- 0.95
BIN_SIZE <- 5
MAX_LITERALS <- 40 
CUTOFF_REDUNDANT <- 0.95
HOLDOUT <- 70

# ************************************************
# NplotOutliers() :
#
# Scatter plot of field values with coloured outliers in pink
#
# INPUT     : data frame      - dataset            - Current dataset
#             integer         - field              - Number of field to plot
#             boolean         - includingOutlier   - If outliers should be included in plot
#             double          - confidence         - Applicable if including outliers
#
# OUTPUT      : None                               - None
# 
# Requires library: outliers
#
# Reference: Code obtained from LAB 3
# ************************************************
NplotOutliers<-function(dataset, field, includingOutlier, confidence){
  
  sortedDataset  <- unique(sort(dataset[,field], decreasing=TRUE))
  
  if (includingOutlier){
    sortedOutliers <- which(outliers::scores(sortedDataset, type="chisq", prob=confidence))
    title = paste("Including Outliers with confidence", confidence)
  }else
  {
    sortedOutliers <- vector()
    title = paste("Excluding Outliers with confidence", confidence)
    }
  
  plot(1:length(sortedDataset), sortedDataset, pch=5, 
       xlab="Unique records", ylab=paste("Sorted values of field: ", names(dataset)[field]), 
       main=title, bty="o")
  if (length(sortedOutliers) > 0)
    points(sortedOutliers, sortedDataset[sortedOutliers],col="pink", pch=18)
}


# ************************************************
# NPREPROCESSING_removeOutliers() :
#
# Determine if a value of a record is an outlier for each NUMERIC field
# ChiSquared method
#
# INPUT     : data frame      - dataset          - Current dataset
#             double          - confidence       - Confidence which determine an outlier [0,1]
#
# OUTPUT    : data frame      - dataset          - Replaced outlier values of NUMERIC fields with mean of field
#
# Requires library: outliers
#
# Reference: Code obtained from LAB 3
# ************************************************
NPREPROCESSING_removeOutliers<-function(dataset, confidence){
  
  for(field in 1:(ncol(dataset))){
    
    # Filter for NUMERIC fields 
    if (is.numeric(dataset[,field])) {
      
      NplotOutliers(dataset, field, TRUE, confidence)
      
      outliers <- which(outliers::scores(dataset[,field], type="chisq", prob=confidence))
      
      # If found records with outlier values, replace those records with mean of field
      if ((length(outliers > 0))){

        dataset[outliers, field] <- round(mean(dataset[,field]))
        
        NplotOutliers(dataset, field, FALSE, confidence)
          
        print(paste("Outlier field=", names(dataset)[field],"Number of outlier records=", length(outliers), "replaced with MEAN"))
      }else
        {
        print(paste("No outliers found for field=", names(dataset)[field])) 
        } 
    }
  }
  return(dataset)
}


# ************************************************
# Nrescale() :
#
# Real values that get scaled between 0-1
# i.e. x-min / (max-min)
#
# INPUT    : double vector         - input            - Values to scale
#
# OUTPUT   : double vector         - output           - Scaled values to [0.0,1.0]
# 
# Reference: Code obtained from LAB 3
# ************************************************
Nrescale <- function(input){
  
  minv <- min(input)
  maxv <- max(input)
  return((input-minv)/(maxv-minv))
}


# ************************************************
# NPREPROCESSING_histogram() :
#
# Plot histoigram for NUMERIC fields 
# Based on observation of dataset, it was decided that all NUMERIC fields
# will be ORDINAL, and not DISCREET
#
# INPUT    : data frame           - dataset     - Current dataset
#            integer              - binsize     - Number of bins in histogram
#
# OUTPUT   : None                               - None
#
# Reference: Code obtained from LAB 3
# ************************************************
NPREPROCESSING_histogram<-function(dataset, binsize){
  
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
      
      # Histogram Plot 
      barplot(bins, main=names(dataset[field]),
              names.arg = 1:binsize, col="light blue", bty="n")
    }
  }
}


# ************************************************
# NPREPROCESSING_rescaleOrdinals() :
#
# Rescle ORDINAL fields to [0.0,1.0]
#
# INPUT      : data frame       - dataset         - Current dataset
#
# OUTPUT     : data frame       - scaledOrdinals  - Numeric data frame with scaled values
#
# Reference: Code obtained from LAB 3
# ************************************************
NPREPROCESSING_rescaleOrdinals<-function(dataset){
  
  # Filter for only NUMERIC fields (all ORDNINALS)
  
  ordinals <- data.frame(NAColumn=rep(NA, nrow(dataset)), stringsAsFactors=FALSE)
  
  for(field in 1:(ncol(dataset))){
    if (is.numeric(dataset[,field])) {
      ordinals <- cbind(ordinals, dataset[,field])
      names(ordinals)[length(ordinals)] <- names(dataset)[field]
    }
  }
  
  ordinals <- ordinals[-1]
  
  zscaled <- as.data.frame(scale(ordinals,center=TRUE, scale=TRUE))
  scaledOrdinals <- sapply(as.data.frame(zscaled),Nrescale)
  
  # write.csv(scaledOrdinals, file="scaled-ORDINALS.csv")
  return(scaledOrdinals)
}


# ************************************************
# NPREPROCESSING_removePunctuation()
#
# INPUT        : character       - fieldName       - Name of field
#
# OUTPUT       : character                         - Name of field with punctuation removed
#
# Reference: Code obtained from LAB 3
# ************************************************
NPREPROCESSING_removePunctuation<-function(fieldName){
  return(gsub("[[:punct:][:blank:]]+", "", fieldName))
}


# ************************************************
# NPREPROCESSING_oneHotCategorical() :
#
# Transform SYMBOLIC  fields using 1-hot-encoding
#
# INPUT         : data frame    - dataset      - Current dataset
#
# OUTPUT        : data frame                   - Categorical data in 1-hot-encoding
#
# Reference: Code obtained from LAB 3
# ************************************************
NPREPROCESSING_oneHotCategorical<-function(dataset, fieldTypes){
  
  # Filter for only SYMBOLIC fields
  
  categoricals <-data.frame(NAColumn=rep(NA, nrow(dataset)), stringsAsFactors=FALSE)
  
  # For seeing what literal is 1 when only 2 unique literals become one field
  binaryTable <- data.frame(NameOfField=NA, LiteralAsOne=NA)
  
  for(field in 1:(ncol(dataset))){

    if (!is.numeric(dataset[,field])) {
      
      # Create a list of unique values in the field (each is a literal)
      literals <- as.vector(unique(dataset[,field]))
      numberLiterals <- length(literals)
      
      # Converts 2 unique literals into one field of {0,1}
      if (numberLiterals==2){
        transformed <- ifelse (dataset[,field]==literals[1], 1.0,0.0)
        binaryTable <-rbind(binaryTable, c(names(dataset)[field], literals[1]))
        
        categoricals <- cbind(categoricals, transformed)
        names(categoricals)[length(categoricals)] <- names(dataset)[field]
        
      } else
      {
        # 1-hot encoding FOR SMALL NUMBER of literals (set max limit)
        if (numberLiterals<=MAX_LITERALS){
          for(num in 1:numberLiterals){
            nameOfLiteral<-literals[num]
            hotEncoding<-ifelse (dataset[,field]==nameOfLiteral, 1.0,0.0)
            
            # Warning - do not convert the field if there are too few literals
            # Use log of number of recrods as the measure
            literalsActive<-sum(hotEncoding==1)
            if (literalsActive>log(length(hotEncoding))) {
              
              categoricals <- cbind(categoricals, hotEncoding)
              names(categoricals)[length(categoricals)] <- paste(names(dataset)[field],
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
          stop(paste("Error - too many literals in:", names(dataset)[field], numberLiterals))
        }
        
      }
    }
  }
  
  write.csv(binaryTable[-1,], "InformationAboutOneHotFields.csv")
  # write.csv(categorical, file="1-hot-encoding-SYMBOLIC.csv")
  return(categoricals[,-1]) 
  
}


# ************************************************
# NcombineDataFrames()
# 
# Combines two dataframes and print out its combined fields
#
# INPUT         : data frame    - dataframe1      - First dataframe
#                               - dataframe2      - Second dataframe
#
# OUTPUT        : data frame                      - Combined dataframes
# ************************************************
NcombineDataFrames<-function(dataframe1, dataframe2){
  
  dataframe <- cbind(dataframe1, dataframe2)
  print(formattable::formattable(data.frame(fields=names(dataframe))))
  
  return(dataframe)
}


# ************************************************
# NPLOTcorrelationMatrix() :
#
# Plots Correlation matrix and saves values in an Excel file
#
# INPUT     : data frame       - dataset        - Current dataset with numeric values only
#
# OUTPUT    : None                              - None
#
# Reference: Code obtained from LAB 3
# ************************************************
NPLOTcorrelationMatrix <- function(dataset){
  
  cr <- cor(dataset, use="everything")
  write.csv(cr, "CorrelationMatrixValues.csv")
  
  pdf("heatMap.pdf", width=30, height=30)
  corrplot::corrplot(abs(cr), method="square", order="FPC", 
                     tl.cex = 0.5, cl.cex = 1.5, mar=c(1,15,1,10))
  dev.off()
  
}


# ************************************************
# NPREPROCESSING_redundantFields() :
#
# Determine if an field is redundant with LINEAR correlation
#  
# INPUT     : data frame       - dataset        - Current dataset with numeric values only
#             double           - cutoff         - Value between [0,1] that above is determined redundant
#
# OUTPUT    : data frame       - dataset        - Dataset with redundant fields removed
#
# Reference: Code obtained from LAB 3
# ************************************************

NPREPROCESSING_redundantFields<-function(dataset, cutoff){

  cr <- cor(dataset, use="everything")
  
  correlated <- which(abs(cr) >= cutoff, arr.ind = TRUE)
  listFieldsCorrelated <- correlated[which(correlated[,1]!=correlated[,2]),]
  
  if (length(listFieldsCorrelated) > 0){
    
    print("Following fields are correlated")
    print(listFieldsCorrelated)
    
    # Check if one of these fields is correlated with another as cant remove both
    v <- vector()
    numc <- nrow(listFieldsCorrelated)
    for (i in 1:numc){
      
      if (length(which(listFieldsCorrelated[i,1] == listFieldsCorrelated[i:numc,2])) == 0) {
        v <- append(v, listFieldsCorrelated[i,1])
      }
    }
    print("Removing the following fields")
    print(names(dataset)[v])
    
    # Remove first field that is correlated with another field 
    return(dataset[, -v])
  }
  return(dataset)
}


# ************************************************
# NPREPROCESSING_randomiseDataset()
# 
# Randomise current dataset records
#
# INPUT         : data frame    - dataset         - Current dataset
#
# OUTPUT        : data frame    - dataset         - Randomised dataset
#
# Reference: Code obtained from LAB 3
# ************************************************
NPREPROCESSING_randomiseDataset<-function(dataset){
  
  dataset <- dataset[order(runif(nrow(dataset))), ]
  return(dataset)
}


# ************************************************
# NPREPROCESSING_balanceDataset()
# 
# Balance current dataset such that there is a 50:50 split on 
# approved vs denied output records
#
# Dataset should be randomised beforehand
#
# INPUT         : data frame    - dataset                  - Current dataset
#                 character     - outputField              - Name of output field
#                 list          - outputCategories         - If dataset is not encoded, include a list
#
# OUTPUT        : data frame    - dataset         - Balanced dataset
# ************************************************
NPREPROCESSING_balanceDataset<-function(dataset, outputField, outputCategories=NA){
  
  if (!is.na(outputCategories)){
    yes <- dataset[dataset[,outputField] == outputCategories[1], ]
    no  <- dataset[dataset[,outputField] == outputCategories[2], ]
  }else{
    # Approved in dataset is equal to 1
    yes <- dataset[dataset[,outputField] == 1, ]
    no  <- dataset[dataset[,outputField] == 0, ]
  }
  
  if (nrow(no) < nrow(yes)){
    yes <- yes[1:nrow(no),]
  }else
  {
    no <- no[1:nrow(yes),]
  }
  
  dataset <- rbind(yes, no)
  print(paste("Rows of denied:", nrow(no), "Rows of approved:", nrow(yes),
              "Total rows in balanced dataset:", nrow(dataset)))
  
  return(dataset)
}


# ************************************************
# NPREPROCESSING_splitDataset() :
#
# Split entire dataset to create training dataset based on HOLDOUT % 
#
# INPUT     : data frame       - dataset      - Current  dataset
#
# OUTPUT    : data frame                       - Test dataset
#             data frame                       - Train dataset
#
# Reference: Code obtained from LAB 3
# ************************************************
NPREPROCESSING_splitDataset<-function(dataset, holdout){

  trainingRecords<-round(nrow(dataset)*(holdout/100))
  trainingData <- dataset[1:trainingRecords,]
  testingData  <- dataset[-(1:trainingRecords),]
  
  retList <- list("TRAINING"=trainingData,
                  "TESTING"=testingData)
  
  return(retList)
}


# ************************************************
# NsplitInputOutput :
#
# Split up dataset to a data frame with the input values 
# and a vector with expected output
#
# INPUT       : data frame      - dataset         - The dataset to split up
#               character       - outputField     - Name of the output field
#
# OUTPUT      : data frame      - inputs          - Columns to insert for the model
#               vector          - expected        - Column of expected output
# ************************************************
NsplitInputOutput<-function(dataset, outputField){
  
  outputPosition <- which(names(dataset) == outputField)
  
  # Data frame with the input fields
  inputs   <- dataset[-outputPosition]
  
  # Vector with the expected output
  expected <- dataset[ ,outputPosition]
  
  retList <- list("INPUT" =inputs,
                  "OUTPUT"=expected,
                  "ALL"   =dataset)
  
  return(retList)
}

