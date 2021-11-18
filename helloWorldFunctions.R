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




