## Load libraries and functions all in one ##

# Load libraries
library(tidyverse)
library(ranger)
library(caret)   # for createFolds
library(varhandle) # for to.dummy()

# Load in our functions
source("methods/ca.R") # CA method
source("methods/ca_binary.R") # Binary method
source("methods/ca_unbiased.R")   # CA method with new levels scored as zero
source("methods/pco.R")           # PCO method
