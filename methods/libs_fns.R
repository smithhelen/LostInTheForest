## Load libraries and functions all in one ##

# Load libraries
library(tidyverse)
library(ranger)
library(caret)   # for createFolds
library(varhandle) # for to.dummy()
library(seqinr) # for Hamming distances
library(stringdist) # for Hamming distances

# Load in our functions
source("methods/ca.R") # CA method
source("methods/binary.R") # Binary method
source("methods/ca_unbiased.R")   # CA method with new levels scored as zero
source("methods/pco.R")           # PCO method
source("methods/tree_predictions.R") # to pull out individual tree predictions
