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

# Clears all objects in Global Environment
rm(list=ls())

# Global Environment variables
RESULTS_FILENAME <- "results.csv"

# sessionInfo()
# Define and Load libraries
# ************************************************
# Library from CRAN     Version
# pacman	               0.5.1
# formattable 	         0.2.1
# PerformanceAnalytics   2.0.4
# openxlsx               4.2.4
# outliers                0.14
# C50                    0.1.5
# randomForest          4.6-14
# pROC                  1.18.0


MYLIBRARIES<-c("formattable",
               "PerformanceAnalytics",
               "openxlsx",
               "outliers",
               "C50",
               "randomForest",
               "pROC")


# h2O requires 64-bit Java.
# Download latest Java SE JDK from the following URL:
# https://www.oracle.com/technetwork/java/javase/downloads/index.html

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

  
  # ************************************************
  # DATA and PREPROCESSING 
  # ************************************************
  
  # Convert the dataset from a CSV file to a Data Frame
  dataset <- NreadDataset(DATASET_FILENAME)
  
  # Remove fields and categories from the output based on data observation
  dataset <- NPREPROCESSING_removeFieldsAndNA(dataset, FIELDS_TO_REMOVE)
  dataset <- NPREPROCESSING_binaryOutput(dataset, OUTPUT_FIELD, OUTPUT_CATEGORIES_TO_REMOVE)
  
  # Present the datasets fields and its effect on the output 
  NPREPROCESSING_presentDataset(dataset)
  NPREPROCESSING_individualEffect(dataset, OUTPUT_FIELD)
  NprintRecordAndFieldCount(dataset)
  
  # Remove outlier records that is not in sync with the rest of the dataset
  dataset <- NPREPROCESSING_removeOutliers(dataset, OUTLIER_CONFIDENCE)
  NPREPROCESSING_histogram(dataset, BIN_SIZE)
  
  # Encode and Scale the fields to numeric values between [0.0, 1.0]
  ordinalReadyforML     <- NPREPROCESSING_rescaleOrdinals(dataset)
  categoricalReadyforML <- NPREPROCESSING_oneHotCategorical(dataset)
  combinedML            <- NcombineDataFrames(ordinalReadyforML,categoricalReadyforML)
  NprintRecordAndFieldCount(combinedML)
  
  # Remove redundant fields based on Linear Correlation
  NPLOTcorrelationMatrix(combinedML)
  combinedML <- NPREPROCESSING_redundantFields(combinedML, CUTOFF_REDUNDANT)  
  NprintRecordAndFieldCount(combinedML)
  
  # First randomisation of dataset before balancing it to 50:50 Approved vs Denied records
  combinedML <- NPREPROCESSING_randomiseDataset(combinedML)
  combinedML <- NPREPROCESSING_balanceDataset(combinedML, OUTPUT_FIELD)
  
  # Second randomisation of dataset before a 70% Training and 30% Testing split 
  combinedML<- NPREPROCESSING_randomiseDataset(combinedML)
  finalizedDataset <- NPREPROCESSING_splitDataset(combinedML, HOLDOUT) 
  
  # Splitting up Training and Testing dataset on its inputs and output
  trainingDataset <- NsplitInputOutput(finalizedDataset$TRAINING, OUTPUT_FIELD)
  testingDataset  <- NsplitInputOutput(finalizedDataset$TESTING, OUTPUT_FIELD)
  
  
  # ************************************************
  # MODELS and EVALUATION
  # ************************************************
  
  title <- paste("Decision Tree C5.0 with boost:", BOOST)
  tree <- NtrainDecisionTree(trainingDataset, BOOST)
  NimportanceOfInput(tree, title, "decisionTree")

  measures <- Nevaluate(tree, testingDataset, OUTPUT_FIELD, "tree", title)
  results <- data.frame(DecisionTree=unlist(measures))


  title <- paste("Random Forest with forest size:", FOREST_SIZE)
  tree <- NtrainRandomForest(trainingDataset, FOREST_SIZE)
  NimportanceOfInput(tree, title, "randomForest")

  measures <- Nevaluate(tree, testingDataset, OUTPUT_FIELD, "tree", title)
  results <- cbind(results, data.frame(RandomForest=unlist(measures)))
  
  
  title <- paste("MLP Neural Network with:", BASICNN_HIDDEN, "neurons")
  neuralNetwork <- NtrainNeuralNetworks(trainingDataset, OUTPUT_FIELD, BASICNN_HIDDEN)
  NimportanceOfInput(neuralNetwork, title, "neuralNetwork")

  measures <- Nevaluate(neuralNetwork, testingDataset, OUTPUT_FIELD, "neuralNetwork", title)
  results <- cbind(results, data.frame(MLPNeuralNetwork=unlist(measures)))
  NPLOTandSummary(neuralNetwork, title)


  title <- paste("Deep Neural Network with:", DEEPNN_HIDDEN[1], "neurons per", DEEPNN_HIDDEN[2], "layer")
  neuralNetwork <- NtrainNeuralNetworks(trainingDataset, OUTPUT_FIELD, DEEPNN_HIDDEN)
  NimportanceOfInput(neuralNetwork, title, "neuralNetwork")

  measures <- Nevaluate(neuralNetwork, testingDataset, OUTPUT_FIELD, "neuralNetwork", title)
  results <- cbind(results, data.frame(DeepNeuralNetwork=unlist(measures)))
  NPLOTandSummary(neuralNetwork, title)
  
  # Comparing classifiers and saving to CSV file
  print(formattable::formattable(results))
  write.csv(t(results), file=RESULTS_FILENAME)
  
  print("END of MAIN")
  
} 


# ************************************************
# Start of R execution

# Garbage Collection
gc()

# Clear console area
cat("\014")


# Clear plots
if(!is.null(dev.list())){
  graphics.off()
}

# Load libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)


# Load complementary R scripts
source("mortgage_approvals_preprocessing_present.R")
source("mortgage_approvals_preprocessing_transform.R")
source("mortgage_approvals_models.R")
source("mortgage_approvals_evaluation.R")

set.seed(123)

# ************************************************
main()

