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

# Trees
BOOST <- 5
FOREST_SIZE <- 10

# Neural Network
EPOCHS            <- 100                  # Maximum number of training epocs

BASICNN_HIDDEN    <- 25                   # Numbr of hidden layer neurons (MLP)
DEEPNN_HIDDEN     <- c(5,10)              # Number of neurons in each layer (deep)

DEEP_STOPPING     <- 2                    # Number of times no improvement before stop
DEEP_TOLERANCE    <- 0.01                 # Error threshold
DEEP_ACTIVATION   <- "TanhWithDropout"    # Non-linear activation function


# ************************************************
# NtrainDecisionTree() :
#
# Create C5 Decision Tree 
#
# INPUT     : data frame     - trainingDataset$INPUT        - Training input dataset
#             vector         - trainingDataset$OUTPUT       - Training output vector
#             integer        - boost                        - Set to 1 is Simple Decision Tree
#                                                             Set to more than 1 is Full Decision Tree
#             boolean        - rules                        - Set to FALSE will enable plot of tree
#
#
# OUTPUT    : tree
#
# Requires library: c50
# ************************************************
NtrainDecisionTree<-function(trainingDataset, boost=1, rules=TRUE){
  
  # Output: if a factor, then assume classification, otherwise regression
  tree <- C50::C5.0(x=trainingDataset$INPUT,
                    y=factor(trainingDataset$OUTPUT),
                    rules=rules,
                    trials=boost)
  
  return(tree)
}   
  
  
# ************************************************
# NtrainRandomForest() :
#
# Create Random Forest
#
# INPUT     : data frame     - trainingDataset$INPUT        - Training input dataset
#             vector         - trainingDataset$OUTPUT       - Training output vector
#             integer        - forestSize                   - Number of trees
#
#
# OUTPUT    : tree
#
# Requires library: randomforest
# ************************************************
NtrainRandomForest<-function(trainingDataset, forestSize){
  
  # Output: if a factor, then assume classification, otherwise regression
  tree <- randomForest::randomForest(x=trainingDataset$INPUT,
                                     y=factor(trainingDataset$OUTPUT),
                                     ntree=forestSize,
                                     importance=TRUE,
                                     mtry=sqrt(ncol(trainingDataset$INPUT)))
  
  return(tree)                     
}


# ************************************************
# NinitialiseH2oServer()
#
# Initializes the H2O server
#
# INPUT       :   None
#
# OUTPUT      :   None
#
# Requires library: h2o
#
# Reference: Code obtained from LAB 4
# ************************************************
NinitialiseH2oServer<-function(){
  
  library(h2o)
  print("Initialising H2O server")
  
  # Number of threads; -1 faster, 1 reproducible results
  h2o.init(max_mem_size = "5g", nthreads = -1)
  
  # Clean in case cluster is already running
  h2o.removeAll()
}


# ************************************************
# NtrainNeuralNetworks()
#
# Create h2O Neural Network 
#
# INPUT    : data frame         - trainingDataset    - Scaled [0.0,1.0] dataset
#            character          - outputField        - Name of the field to classify
#            integer vector     - hiddenLayers       - Number of hidden layer neurons for each layer
#
# OUTPUT   : classifier
#
# Requires library: h2o
#
# Reference: Code obtained from LAB 4
# ************************************************
NtrainNeuralNetworks<-function(trainingDataset, outputField, hiddenLayers){
  
  NinitialiseH2oServer()
  
  # Output: if a factor, then assume classification, otherwise regression
  trainingDataset$ALL[outputField] <- lapply(trainingDataset$ALL[outputField] , factor)
  train_h2o <- as.h2o(trainingDataset$ALL, destination_frame = "traindata")
  
  # Create validation dataset for early stopping
  splits   <- h2o.splitFrame(train_h2o, 0.9, seed=1234)
  nntrain  <- h2o.assign(splits[[1]], "nntrain.hex") # 90%
  nnvalid  <- h2o.assign(splits[[2]], "nnvalid.hex") # 10%
  
  neuralNetwork <-h2o::h2o.deeplearning(x=names(trainingDataset$INPUT),
                                        y=outputField,
                                        training_frame = nntrain,
                                        validation_frame=nnvalid,
                                        epochs=EPOCHS,
                                        hidden=hiddenLayers,
                                        adaptive_rate=TRUE,
                                        stopping_rounds=DEEP_STOPPING,
                                        stopping_tolerance=DEEP_TOLERANCE,
                                        stopping_metric = "misclassification",
                                        fast_mode=FALSE,
                                        activation=DEEP_ACTIVATION,
                                        seed=1234,
                                        l1 = 1e-2,
                                        l2 = 1e-2,
                                        variable_importances = TRUE,
                                        reproducible = TRUE)
  
  return(neuralNetwork)
}

