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

# Clears all objects in Global Environment
rm(list=ls())

# Global Environment variables
DATASET_FILENAME <- "Washington_State_HDMA-2016.csv"
OUTPUT_FIELD      <- "action_taken_name"
# 13 = respondent_id (had 753 categories)
FIELDS_TO_REMOVE <- c(2, 10, 11, 13, 24:26, 29:32, 36, 39:42, 45)
HOLDOUT           <- 70 
BINSIZE = 10
CUTOFF = 5
CUTOFF_OUTLIER    <- 0.99                 # Confidence p-value for outlier detection
# Set to negative means analyse but do not replace outliers
CUTOFF_DISCREET   <- 5                    # Number of empty bins to determine discreet
CUTOFF_REDUNDANT  <- 0.95       
OUTLIER_CONF      <- 0.95
TYPE_SYMBOLIC <- "SYMBOLIC"
TYPE_DISCREET <- "DISCREET"
TYPE_ORDINAL  <- "ORDINAL"
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"
manualTypes <- data.frame()

# Maximum number of 1-hot-coding new fields
MAX_LITERALS <- 50                 



# !!! CHANGE THIS WHEN WE KNOW WHAT WE NEED
# Define and Load libraries
# ************************************************
# Library from CRAN     Version
# pacman	               0.5.1
# outliers	             0.14
# corrplot	             0.84
# MASS	                 7.3.51.4
# pROC	                 1.15.3
# formattable 	         0.2.01
# stats                  3.6.1
# PerformanceAnalytics   1.5.3


# install.packages("openxlsx")
#          library("openxlsx")

MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "pROC",
               "formattable",
               "stats",
               "PerformanceAnalytics",
               "openxlsx")






# 1. read dataset [DONE]
#
# 2. determine type of fields (NUMERIC or SYMBOLIC) [DONE]
#
# NUMERIC: 
#   a. determine if ORDINAL or DISCRETE [DONE]

#   b. determine if any ORDINAL fields are outliers
#      replace outlier value with mean of the field
#   c. transform dataset using z-scale

#   d. ensure dataset is scaled to values classifer requires [0.0, 1.0] (use the Nrescale)
#
# SYMBOLIC:
#   a. transform using 1-hot-encoding (its own field, with values {0,1}) [DONE ish]



# (create a single dataset?)


# (have done it with the removal already?)
# 3. Reduce dimensionality:
#       determine if any fields are redundant using correlation
#       remove fields that are strongly correlated (do not provide extra info)
#    
# 
# A. randomize dataset
# B. split dataset into training and testing 
# (The data is now prepared for training a classifier) 









# ************************************************
# main() :
#
# Entry point to execute analytics
#
# INPUT       :   None
#
# OUTPUT      :   None
# ************************************************
main<-function(){
  
  print("START of MAIN")
  
  dataset <- NreadDataset(DATASET_FILENAME)
  
  dataset <- NcleanDataset(dataset, FIELDS_TO_REMOVE)
  
  NPREPROCESSING_presentDataset(dataset)
  
  # need to bucket the discrete ones into a category 
  fieldTypes = NPREPROCESSING_fieldTypes(dataset, BINSIZE, CUTOFF)
  
  
  
  # ordinalReadyForML = 
  
  
  # at the moment only for SYMBOLIC
  #ordinals<-dataset[,which(fieldTypes==TYPE_ORDINAL)]
  
  # Test if any ordinals are outliers and replace with mean values
  # Null hyposis is there are no outliers
  # We reject this if the p-value<significance (i.e. 0.05), confidence=95%
  
  #ordinals<-NPREPROCESSING_outlier(ordinals=ordinals,confidence=OUTLIER_CONF)
  
  # ************************************************
  # z-scale
  #zscaled<-as.data.frame(scale(ordinals,center=TRUE, scale=TRUE))
  
  # In the choosen classifier, the input values need to be scaled to [0.0,1.0]
  #ordinalReadyforML<-Nrescaleentireframe(zscaled)
  #catagoricalReadyForML = NPREPROCESSING_categorical(dataset, fieldTypes)
  #combinedML<-cbind(ordinalReadyforML,catagoricalReadyForML)
  #combinedML<-NPREPROCESSING_redundantFields(dataset=combinedML,cutoff=0.95)
  #nonNumericbefore<-length(which(fieldTypes!=TYPE_ORDINAL))
  
  # How many fields have be generated through the 1-hot-encoding process
 # nonNumerictranformed<-ncol(catagoricalReadyForML)
 # print(paste("Symbolic fields. Before encoding=",nonNumericbefore,"After",nonNumerictranformed))
  # combinedML (join the 2 dataframes)
  
  
  #combinedML<-combinedML[order(runif(nrow(combinedML))),]
  
  # Create a TRAINING dataset using first HOLDOUT% of the records
  # and the remaining 30% is used as TEST
  # use ALL fields (columns)
  #training_records<-round(nrow(combinedML)*(HOLDOUT/100))
  #training_data <- combinedML[1:training_records,]
  #testing_data = combinedML[-(1:training_records),]
  
  # ************************************************
  
  #myModelling(training_data = training_data, testing_data = testing_data)
  
  dataset<-NPREPROCESSING_dataset(dataset=dataset,
                                  scaleFlag=TRUE)
  
  # Holdout split of the dataset
  # Randomise the dataset and then split it into train and test
  splitData<-NPREPROCESSING_splitdataset(dataset)
  
  # ************************************************
  # Experiment with C5 decision tree and preprocessed dataset
  measures<-fullDT(train=splitData$train, test=splitData$test)
  
  
  
  print("END of MAIN")
  
} 


# ************************************************
# Start of R execution

# Garbage Collection
gc()

# Clear console area
cat("\014")

# Load libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)


# Load additional R scripts
source("helloWorldFunctions.R")

set.seed(123)

# ************************************************
main()