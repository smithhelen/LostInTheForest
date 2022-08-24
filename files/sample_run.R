# Load libraries and functions
source("methods/libs_fns.R")

# Load sample data 
# data is genetic data with alleles (levels) from 10 genes (variables) across 3 sources (classes), n=10 for each source
load("data/LostInTheForest.RData") # sample data
load("data/SeqDat.RData") # sequence information for sample data

# Prepare data
# calculate hamming distances between levels of predictor variable
source("methods/hamming.R")
genes <- names(SeqDat %>% select(starts_with("Var")))
list_of_distance_matrices <- map(genes, ~ dfun(gene=., dat=LostInTheForest, seqdat=SeqDat))
names(list_of_distance_matrices) <- genes
list_of_distance_matrices <- map(list_of_distance_matrices, as.matrix)

# Training data is the source data
Dat.train <- LostInTheForest %>% filter(class != "Human") %>% droplevels() %>% mutate(across(everything(), factor)) 

# Test data is the human data to be attributed to a source
Dat.test <- LostInTheForest %>% filter(class == "Human") %>% droplevels() %>% mutate(across(everything(), factor)) 

# Source attribution of the sample data using the different methods
#ca
train <- prepare_training_ca(Dat.train, starts_with("Var"), "class")
test <- prepare_test_ca(Dat.test)
ca_mod <- ranger(Class ~ ., data=train$training, oob.error = TRUE, num.trees=500, respect.unordered.factors = TRUE)
Prediction <- predict(ca_mod, data=test, predict.all = FALSE)$predictions
table(Prediction) %>% as.data.frame() # counts of predictions for each source

#ca_binary
train <- prepare_training_ca_binary(Dat.train, starts_with("Var"), "class")
test <- prepare_test_ca_binary(Dat.test, train$extra, id)
binary_mod <- ranger(dependent.variable.name = "Class", data = train$training, oob.error = TRUE, num.trees=500, respect.unordered.factors = TRUE)
Prediction <- predict(binary_mod, data=test, predict.all = FALSE)$predictions
table(Prediction) %>% as.data.frame() # counts of predictions for each source

#ca_unbiased
train <- prepare_training_ca0(Dat.train, starts_with("Var"), "class")
test <- prepare_test_ca0(Dat.test, train$extra, id)
ca0_mod <- ranger(Class ~ ., data=train$training, oob.error = TRUE, num.trees=500, respect.unordered.factors = TRUE)
Prediction <- predict(ca0_mod, data=test, predict.all = FALSE)$predictions
table(Prediction) %>% as.data.frame() # counts of predictions for each source

#pco
train <- prepare_training_pco(Dat.train, starts_with("Var"), "class", list_of_distance_matrices)
test <- prepare_test_pco(Dat.test, train$extra, id)
pco_mod <- ranger(Class ~ ., data=train$training, oob.error = TRUE, num.trees=500, respect.unordered.factors = TRUE)
Prediction <- predict(pco_mod, data=test, predict.all = FALSE)$predictions
table(Prediction) %>% as.data.frame() # counts of predictions for each source





