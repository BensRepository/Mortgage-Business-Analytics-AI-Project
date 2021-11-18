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
                        Name=0,
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
      
      # Counts each category in a SYMBOLIC field
      categoryCount <- sapply(unique(dataset[,field]),function(x) length(which(dataset[,field]==x)))
      majorityCategoryPC <- round((sort(categoryCount, decreasing = TRUE)[1]/nrow(dataset))*100,digits=0)
      tidyTable$Name[field] <- paste(names(majorityCategoryPC),"(",majorityCategoryPC,"%)",sep="")
      
      # Saves the result in a Excel file
      df <- data.frame(Category=names(categoryCount), Amount=categoryCount)
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


