## Load libraries and functions for Lost In The Forest methods ##

# Load libraries
library(tidyverse)
library(ranger)
library(recipes)
library(rsample) # for initial_split()
library(tidymodels)
#library(finetune)
#library(caret)   # for createFolds

# Load in our functions
source("methods/ca.R")                # CA method
source("methods/ca0.R")       # CA method with new levels scored as zero
source("methods/pco.R")               # PCO method
source('methods/recipe_ca.R')         # CA recipe
source('methods/recipe_ca0.R')        # CA unbiased recipe
source("methods/recipe_pco.R")        # PCO recipe
