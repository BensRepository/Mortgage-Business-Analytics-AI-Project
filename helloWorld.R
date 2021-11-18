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
FIELDS_TO_REMOVE <- c(2, 10, 11, 24:26, 29:32, 36, 39:42, 45)





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
#   a. determine if ORDINAL or DISCRETE
#   b. determine if any ORDINAL fields are outliers
#      replace outlier value with mean of the field
#   c. transform dataset using z-scale
#   d. ensure dataset is scaled to values classifer requires [0.0, 1.0]
#
# SYMBOLIC:
#   a. transform using 1-hot-encoding (its own field, with values {0,1})
#
# 3. Reduce dimensionality: 
#       determine if any fields are redundant using correlation
#       remove fields that are strongly correlated (do not provide extra info)
#    
# 4. Split dataset into training and testing
#
#
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
  
  # MODIFICATION to be done:
  #   column for ORDINAL or DISCRETE 
  #   create csv file with all the categories DONE
  NPREPROCESSING_presentDataset(dataset)
  
  fieldTypes = NPREPROCESSING_fieldTypes(dataset, 10, 5)
  
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