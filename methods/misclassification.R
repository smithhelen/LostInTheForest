#' functions to calculate misclassification rates of the individual trees according to the presence of absent levels 
#' as well as overall misclassification rates of the whole forests for the different methods 

source("methods/tree_predictions.R")

# (1) - calculate proportion correct predictions for trees with and without unique alleles
tree_predictions <- function(data, model){
  train <- map(model, "train")
  test <- map(model, "test")
  class <- map(model, "class")
  id <- map(model, "id")
  ranger_mod <- map(model, "ranger_mod")
  uniques <- is_unique(data, train$extra)
  tree_preds <- predict_by_tree(ranger_mod, test, uniques, id=id)
  tree_preds |> left_join(data |> rename(id = id) |> select(id, any_of(class)))
}

tree_misclassifications <- function(data, model, class){
  tree_preds <- map2_df(data, model, tree_predictions)
  df <- tree_preds |> 
    mutate(uses_unique = if_else(uses_unique == 0, "No", "Yes")) |>
    group_by(method,uses_unique, class, prediction) |>
    summarise(n=n()) |>
    group_by(method,uses_unique, class) |>
    mutate(N = sum(n)) |>
    mutate(prop = n/N)
  df
}


# (2) - calculate proportion correct classifications for forests (ie calculate misclassification rate)
forest_predictions <- function(data, model){
  test <- map(model, "test")
  class <- map(model, "class")
  id <- map(model, "id")
  ranger_mod <- map(model, "ranger_mod")
  preds_ranger <- predict(ranger_mod, data=test, predict.all = FALSE)$predictions
  forest_preds <- data.frame(id = test |> pull(id), preds = preds_ranger) |> left_join(data |> rename(id = id) |> select(id, any_of(class)))
  forest_preds
}


forest_misclassifications <- function(testdata, model, class) {
  forest_preds <- map2(testdata, model, forest_predictions)
  
  # weights for each fold
  w <- forest_preds |> group_by(fold) |> tally() |> pull(n)
  
  # misclassification rates of the folds
  lm <- forest_preds |> 
    rename(truths = class) |> 
    mutate(wrong = truths != preds) |>
    group_by(fold) |>
    summarise(miss = sum(wrong)/n()) |> 
    lm(miss ~ 1, weights = w, data=_) |> summary()
  
  # weighted mean and standard error of the misclassification rates
  av <- coef(lm)[,"Estimate"]
  se <- coef(lm)[,"Std. Error"]
  
  # confusion matrix
  conf <- forest_preds |> 
    rename(truths = class) |> 
    group_by(fold, truths, preds) |> 
    summarise(n=n()) |> 
    mutate(N=sum(n), p=n/sum(n)) |> 
    ungroup() |>
    mutate(w = as.numeric(paste(factor(fold, labels=w))), t_p = paste(truths, preds, sep="_")) |> 
    split(~t_p)
  
  # weighted mean and standard error of the confusion matrices
  conf.lm <- map(conf, function(x) {lm(p~1, weights=w, data=x) |> summary()})
  conf.av <- map(conf.lm, function(x) coef(x)[,"Estimate"])
  conf.se <- map(conf.lm, function(x) coef(x)[,"Std. Error"])
  
  out <- list(av=av, se=se, conf.av=conf.av, conf.se=conf.se)
  out
}

