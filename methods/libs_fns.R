## Load libraries and functions all in one - Lost In The Forest methods ##

# Load libraries
library(tidyverse)
library(ranger)
library(caret)   # for createFolds

# Load in our functions
source("methods/ca.R")                # CA method
source("methods/binary.R")            # Binary method
source("methods/ca_unbiased.R")       # CA method with new levels scored as zero
source("methods/pco.R")               # PCO method
source("methods/similarity.R")        # Similarity method
