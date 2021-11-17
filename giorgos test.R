rm(list=ls())

Global_train_inputs <- NA
Global_train_inputs_expected <- NA
DATASET_FILENAME  <- "Washington_State_HDMA-2016.csv" #Name of input dataset file
MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "pROC",
               "formattable",
               "stats",
               "caret",
               "PerformanceAnalytics",
               "stringr",
               "partykit",
               "C50",
               "randomForest",
               "keras",
               "h2o")

# ************************************************
# main() :
# main entry point to execute analytics
#
# INPUT       :   None
#
# OUTPUT      :   None
#
# Keeps all objects as local to this function
# ************************************************
main<-function(){
  print("Inside Main function")
  loans<-NreadDataset(DATASET_FILENAME)
  NPREPROCESSING_prettyDataset(loans)
}#endof main()

# ************************************************
# This is where R starts execution

# clears the console area
cat("\014")

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#This [optionally] sets working directory
#setwd("/Users/nickryman-tubb/Documents/My Stuff/University of Surrey/UOS Teaching/MANM354 - 2019/Labs/lab3 - preprocessing/code")

#Load additional R script files provide for this lab
source("Preprocessing.R")

set.seed(123)

# ************************************************
main()

print("end")


