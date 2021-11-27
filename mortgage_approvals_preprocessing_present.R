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

DATASET_FILENAME <- "Washington_State_HDMA-2016.csv"

# keep   = 1
# remove = 0

# [1] "1 tract_to_msamd_income"
# [0] "2 rate_spread"
# [1] "3 population"
# [1] "4 minority_population"
# [1] "5 number_of_owner_occupied_units"
# [1] "6 number_of_1_to_4_family_units"
# [1] "7 loan_amount_000s"
# [0] "8 hud_median_family_income"
# [1] "9 applicant_income_000s"
# [0] "10 state_name"
# [0] "11 state_abbr"
# [0] "12 sequence_number"
# [0] "13 respondent_id"
# [0] "14 purchaser_type_name"
# [1] "15 property_type_name"
# [1] "16 preapproval_name"
# [1] "17 owner_occupancy_name"
# [1] "18 msamd_name"
# [1] "19 loan_type_name"
# [1] "20 loan_purpose_name"
# [1] "21 lien_status_name"
# [0] "22 hoepa_status_name"
# [0] "23 edit_status_name"
# [0] "24 denial_reason_name_3"
# [0] "25 denial_reason_name_2"
# [0] "26 denial_reason_name_1"
# [1] "27 county_name"
# [1] "28 co_applicant_sex_name"
# [0] "29 co_applicant_race_name_5"
# [0] "30 co_applicant_race_name_4"
# [0] "31 co_applicant_race_name_3"
# [0] "32 co_applicant_race_name_2"
# [1] "33 co_applicant_race_name_1"
# [1] "34 co_applicant_ethnicity_name"
# [0] "35 census_tract_number"
# [0] "36 as_of_year"
# [0] "37 application_date_indicator"
# [1] "38 applicant_sex_name"
# [0] "39 applicant_race_name_5"
# [0] "40 applicant_race_name_4"
# [0] "41 applicant_race_name_3"
# [0] "42 applicant_race_name_2"
# [1] "43 applicant_race_name_1"
# [1] "44 applicant_ethnicity_name"
# [0] "45 agency_name"
# [1] "46 agency_abbr"
# [1] "47 action_taken_name"

FIELDS_TO_REMOVE <- c(2L, 8L, 10L, 11L, 12L, 13L, 14L, 22L, 23L, 24:26L, 29:32L, 35L, 36L, 37L, 39:42L, 45L)


# [1] "Loan originated"                                    
# [0] "Application approved but not accepted"              
# [1] "Application denied by financial institution"        
# [0] "Application withdrawn by applicant"                 
# [0] "File closed for incompleteness"                     
# [0] "Loan purchased by the institution"                  
# [0] "Preapproval request denied by financial institution"
# [0] "Preapproval request approved but not accepted"

# Reduce the output categories to binary; Yes accepted or No rejected
OUTPUT_FIELD <- "action_taken_name"

OUTPUT_CATEGORIES_TO_REMOVE <- c("Application approved but not accepted",
                                 "Application withdrawn by applicant",
                                 "File closed for incompleteness",
                                 "Loan purchased by the institution",
                                 "Preapproval request denied by financial institution",
                                 "Preapproval request approved but not accepted")


OUTPUT_CATEGORIES_TO_KEEP <- c("Loan originated",
                               "Application denied by financial institution")

# ************************************************
# NreadDataset() :
#
# Read a CSV file from working directory
#
# INPUT       : string      - csvFilename   - Name of CSV file
#
# OUTPUT      : data frame  - dataset       - Contents of CSV file
# 
# Reference: Code obtained from LAB 3
# ************************************************
NreadDataset<-function(csvFilename){
  
  dataset<-read.csv(csvFilename, encoding="UTF-8", stringsAsFactors = FALSE)
  
  print(paste("CSV dataset", csvFilename, "has been read. Records=", nrow(dataset)))
  return(dataset)
}


# ************************************************
# NPREPROCESSING_removeFieldsAndNA() :
#
# Removes unwanted fields and records with NA
#
# INPUT       : data frame      - dataset               - Dataset read from CSV file
#               integer vector  - fieldsToRemove        - List of fields to remove            
#
# OUTPUT      : data frame      - dataset               - Dataset without unwanted fields and NA
# ************************************************
NPREPROCESSING_removeFieldsAndNA<-function(dataset, fieldsToRemove){
  
  dataset <- dataset[-fieldsToRemove]
  dataset <- na.omit(dataset)
  
  return(dataset)
}


# ************************************************
# NPREPROCESSING_binaryOutput() :
#
# Removes categories from the output field until it is binary
#
# INPUT       : data frame        - dataset               - Dataset read from CSV file
#               character         - outputField           - Name of output field
#               character vector  - categorisToRemove     - List of categories to remove            
#
# OUTPUT      : data frame        - dataset               - Dataset with binary output field
# ************************************************
NPREPROCESSING_binaryOutput<-function(dataset, outputField, categorisToRemove){
  
  for (c in categorisToRemove){
    dataset <- dataset[dataset[,outputField] != c,]
  }
  
  if (length(unique(dataset[,outputField])) != 2){
    stop("Output field not binary")
  }
  
  return(dataset)
}


# ************************************************
# NprintRecordAndFieldCount() :
#
# Prints rrecord and field count of current dataset
#
# INPUT       : data frame        - dataset               - Dataset read from CSV file
#
# OUTPUT      : None                                      - None
# ************************************************
NprintRecordAndFieldCount<-function(dataset){
  
  print(paste("Current Field Count =", length(dataset)))
  print(paste("Current Record Count = ", nrow(dataset)))
  
}


# ************************************************
# NPREPROCESSING_presentDataset()
# 
# Displays dataset field analysis results as a table in "Viewer"
# Creates an Excel file with statistics over the SYMBOLIC categories
#
#
# INPUT           : data frame   - dataset              - Full dataset used for train/test
#                                                         Each row is one record, each column in named
#                                                         Values are not scaled or encoded
#
# OUTPUT          : None                                - None
#                                                         
# Requires library: PerformanceAnalytics
#                   formattable
#                   openxlsx
# 
# Reference: Code obtained from LAB 3
# ************************************************
NPREPROCESSING_presentDataset<-function(dataset){
  
  wb <- openxlsx::createWorkbook()
  
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
      openxlsx::addWorksheet(wb, names(dataset)[field])
      openxlsx::writeData(wb, names(dataset)[field], df)
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
  openxlsx::saveWorkbook(wb, file="CategoryStatistcs.xlsx", overwrite=TRUE)
}


# ************************************************
# NPREPROCESSING_individualEffect() :
#
# Plots individual effect on the output for SYMBOLIC fields
#
# INPUT       : data frame        - dataset               - Dataset read from CSV file
#               character         - outputField           - Name of output field
#
# OUTPUT      : None                                      - None
#
# Requires library: openxlsx
# ************************************************
NPREPROCESSING_individualEffect<-function(dataset, outputField){
  
  wb <- openxlsx::createWorkbook()
  
  dataset <- NPREPROCESSING_randomiseDataset(dataset)
  dataset <- NPREPROCESSING_balanceDataset(dataset, outputField, OUTPUT_CATEGORIES_TO_KEEP)
    
  # Increase margins
  par(mar=c(5, 15, 5, 2))
  
  for(field in 1:(ncol(dataset))){
    
    # Filter for SYMBOLIC fields 
    if (!is.numeric(dataset[,field])) {
      
      stats <- table(dataset[,outputField], dataset[,field])
      barplot(stats, col=c("pink","light green"), legend=c("Denied", "Approved"),
              main=names(dataset)[field],
              horiz=T, las=1)
      
      # Saves the result in a Excel file
      openxlsx::addWorksheet(wb, names(dataset)[field])
      openxlsx::writeData(wb, names(dataset)[field], stats)
      
    }
    
  }
  openxlsx::saveWorkbook(wb, file="IndividualEffectStatistcs.xlsx", overwrite=TRUE)
  
  # Default margins  
  par(mar=c(5.1, 4.1, 4.1, 2.1))
}

