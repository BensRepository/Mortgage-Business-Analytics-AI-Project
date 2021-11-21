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

# 13 = respondent_id (had 753 categories)
FIELDS_TO_REMOVE <- c(2, 8, 10, 11, 12, 13, 22, 23, 24:26, 29:32, 35, 36, 37, 39:42, 45)

BINSIZE = 20
CUTOFF = 20

CUTOFF_OUTLIER <- 0.99
CUTOFF_REDUNDANT <- 0.95

TYPE_SYMBOLIC <- "SYMBOLIC"
TYPE_DISCREET <- "DISCREET"
TYPE_ORDINAL  <- "ORDINAL"

# Maximum number of 1-hot-coding new fields
MAX_LITERALS <- 50                 

scaleFlag <- TRUE

HOLDOUT <- 70

OUTPUT_FIELD <- "action_taken_name"


PDF_FILENAME      <- "tree.pdf"           # Name of PDF with graphical tree diagram
RULES_FILENAME    <- "rules.txt"          # Name of text file with rules saved
RESULTS_FILENAME  <- "results.csv"        # Name of the CSV results file
NODE_LEVEL        <- 1                    # The number is the node level of the tree to print
BOOST             <- 20                   # Number of boosting iterations. 1=single model
FOREST_SIZE       <- 10                 # Number of trees in the forest
SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage




BASICNN_HIDDEN    <- 10                   # 10 hidden layer neurons
BASICNN_EPOCHS    <- 100                  # Maximum number of training epocs


DEEP_HIDDEN       <- c(5,5)               # Number of neurons in each layer
DEEP_STOPPING     <- 2                    # Number of times no improvement before stop
DEEP_TOLERANCE    <- 0.01                 # Error threshold
DEEP_ACTIVATION   <- "TanhWithDropout"    # Non-linear activation function
DEEP_REPRODUCABLE <- TRUE                 # Set to TRUE to test training is same for each run


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
               "openxlsx",
               
               
               "caret",
               "stringr",
               "partykit",
               "C50",
               "randomForest",
               "keras",
               "h2o"
               )






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
  fieldTypes <- NPREPROCESSING_fieldTypes(dataset, BINSIZE, CUTOFF)
  
  
  ordinals <- dataset[,which(fieldTypes=="ORDINAL"),drop=FALSE]
  ordinals <- NPREPROCESSING_outlier(ordinals, CUTOFF_OUTLIER)
  
  
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
  
  
  
  # at the moment only for SYMBOLIC
  categoricalReadyforML <- NPREPROCESSING_categorical(dataset, fieldTypes)
  
  
  # combinedML (join the 2 dataframes)
  combinedML<-cbind(ordinalReadyforML,categoricalReadyforML)
  
  
  combinedML<-NPREPROCESSING_redundantFields(dataset=combinedML,cutoff=CUTOFF_REDUNDANT)
  
  
  print(paste("Fields=",ncol(combinedML)))
  names(combinedML)<-gsub(" ", "", names(combinedML), fixed = TRUE)
  
  
  original <- NPREPROCESSING_splitdataset(combinedML)
  # myModelling(original$train, original$test)
  
  
  
  
  
  #########################################################
  

  original1<-NConvertClass(combinedML)
  
  # ************************************************
  # Without any pre-processing of the dataset
  # Decision trees are good for this!
  original1<-NPREPROCESSING_splitdataset(original1)
  
  measures<-simpleDT(original1$train,original1$test)
  
  # Create a data frame to compare results from different experiments
  allResults<-data.frame(DT_raw=unlist(measures))
  
  
  
#############################################################
  
  
  # Try a random forest model
  measures<-randomForest(train=original$train, test=original$test)
  # measures<-runExperiment(dataset = dataset,FUN = randomForest)
  
  # Keep a note of all our results - append to the all results
  allResults<-cbind(allResults,data.frame(RandomForest=unlist(measures)))
  
  
  
  
 ################################################################### 
  
  
  # Now a MLP neural network
  measures<-mlpNeural(train=original$train, test=original$test)
  
  # Keep a note of all our results - append to the all results
  allResults<-cbind(allResults,data.frame(MLP=unlist(measures)))
  
  
  
  
  
  
  # ************************************************
  # Now a deep neural network
  measures<-deepNeural(train=original$train, test=original$test)
  
  # Keep a note of all our results - append to the all results
  allResults<-cbind(allResults,data.frame(Deep_Neural=unlist(measures)))
  
  
  
  
  
  
  
  allResults<-data.frame(t(allResults))
  
  # Sort by highest MCC
  allResults<-allResults[order(allResults$MCC,decreasing = TRUE),]
  
  # Output results to compare all classifiers
  print(formattable::formattable(allResults))
  
  # Write frame to a CSV files
  write.csv(allResults,file=RESULTS_FILENAME)
  
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