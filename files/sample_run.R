### Source attribution of the sample data ###

## Load libraries and functions
source("methods/libs_fns.R")
source("methods/tree_predictions.R")


## Load sample data 
# data is genetic data with alleles (levels) from 10 genes (variables) across 3 sources (classes), n=10 for each source
load("data/LostInTheForest.RData") # sample data
load("data/SeqDat.RData") # sequence information for sample data


## Prepare data
# calculate hamming distances between levels of predictor variable
source("methods/hamming.R")
genes <- names(SeqDat |> select(starts_with("Var")))
list_of_distance_matrices <- map(genes, ~ dfun(gene=., dat=LostInTheForest, seqdat=SeqDat))
names(list_of_distance_matrices) <- genes
list_of_distance_matrices <- map(list_of_distance_matrices, as.matrix)

# Training data is the source data
Dat.train <- LostInTheForest |> filter(class != "Human") |> droplevels() |> mutate(across(everything(), factor)) 

# Test data is the human data to be attributed to a source
Dat.test <- LostInTheForest |> filter(class == "Human") |> droplevels() |> mutate(across(everything(), factor)) 

set.seed(123)

## Define random forest model
rf_mod <- 
  rand_forest(trees = 500)  |>  
  set_engine("ranger", respect.unordered.factors = TRUE, probability = FALSE) |> 
  set_mode("classification")

# base recipe
base_recipe <- recipe(class ~ ., data=Dat.train) |> 
  update_role(id, new_role = "id")

# update recipe according to chosen method
# 1. ca method
rf_recipe <- base_recipe |> step_ca(starts_with("Var")) 

# 2. ca unbiased method
rf_recipe <- base_recipe |> step_ca_unbiased(starts_with("Var")) 

# 3. pco method
rf_recipe <- base_recipe |> step_pco(starts_with("Var"), distances = list_of_distance_matrices) 

# once recipe is updated:
# encode the variables
prepped_recipe <- rf_recipe |> prep(strings_as_factors=FALSE)
baked_train <- prepped_recipe |> juice()
baked_test <- prepped_recipe |> bake(Dat.test)

# fit model
rf_fit <- rf_mod |> fit(class  ~ ., data = baked_train |> select(class,starts_with("Var")))

# predict human data (forest predictions)
rf_preds <- baked_test |> select(id) |> bind_cols(rf_fit |> predict(baked_test)) |> rename(prediction=.pred_class)
table(rf_preds$prediction) %>% as.data.frame() # counts of predictions for each source

# individual tree predictions
step_objects <- prepped_recipe$steps[[1]]$objects |> compact()
uniques <- is_unique(Dat.test, step_objects)
tree_preds <- predict_by_tree(rf_fit$fit, new_data=baked_test, new_unique=uniques, id="id")
table(tree_preds$prediction) %>% as.data.frame() # counts of predictions for each source
