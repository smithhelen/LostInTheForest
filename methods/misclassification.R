#### functions to calculate misclassification rates of the individual trees according to the presence of absent levels 
#### as well as overall misclassification rates of the whole forests for the different methods ####

source("methods/tree_predictions.R")

# functions
# # (1) - calculate proportion correct predictions for trees with and without unique alleles
# misclass_tree_fn <- function(dat, class){
#   df <- dat |> 
#     mutate(uses_unique = if_else(uses_unique == 0, "No", "Yes")) |>
#     group_by(method,uses_unique, {{class}}, prediction) |>
#     summarise(n=n()) |>
#     group_by(method,uses_unique, {{class}}) |>
#     mutate(N = sum(n)) |>
#     mutate(prop = n/N)
#   df
# }
  
# (2) - calculate proportion correct classifications for forests (ie calculate misclassification rate)
MC_fn <- function(Dat.train, Dat.test, d=NULL, axes=2, method="ca0", ntrees=500, id, class, var_id){
  switch(method, 
         ca = {
           train <- prepare_training_ca(Dat.train, starts_with(var_id), class=class)
           test <- prepare_test_ca(Dat.test, train$extra, id=id)
         },
         binary = {
           train <- prepare_training_ca_binary(Dat.train, starts_with(var_id), class=class)
           test <- prepare_test_ca_binary(Dat.test, train$extra, id=id)
         },
         ca0 = {
           train <- prepare_training_ca0(Dat.train, starts_with(var_id), class=class, axes=axes)
           test <- prepare_test_ca0(Dat.test, train$extra, id=id)
         },
         pco = {
           train <- prepare_training_pco(Dat.train, starts_with(var_id), class=class, d, axes=axes)
           test <- prepare_test_pco(Dat.test, train$extra, id=id)
         }
  )       
  classes <- train$training |> pull(class)
  ranger_mod <- if(method=="binary") {
    ranger(dependent.variable.name = class, data = train$training, oob.error = TRUE, num.trees=ntrees, respect.unordered.factors = TRUE)
  } else {
    ranger(classes ~ ., data=train$training |> select(-any_of(class)), oob.error = TRUE, num.trees=ntrees, respect.unordered.factors = TRUE)
  }
  preds_ranger <- predict(ranger_mod, data=test, predict.all = FALSE)$predictions
  df <- data.frame(id = test |> pull({{id}}), preds = preds_ranger) |> left_join(Dat.test |> rename(id = {{id}}) |> select(id, any_of(class)))
  df
}



calc_misclassification <- function(df, class) {
  # weights for each fold
  w <- df |> group_by(fold) |> tally() |> pull(n)
  
  # misclassification rates of the folds
  lm <- df |> 
    rename(truths = class) |> 
    mutate(wrong = truths != preds) |>
    group_by(fold) |>
    summarise(miss = sum(wrong)/n()) |> 
    lm(miss ~ 1, weights = w, data=_) |> summary()
  
  # weighted mean and standard error of the misclassification rates
  av <- coef(lm)[,"Estimate"]
  se <- coef(lm)[,"Std. Error"]
  
  # confusion matrix
  conf <- df |> 
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

misclass_fn <- function(Dat.train, Dat.test, method="ca0", class, id, d=NULL, axes=2, ntrees=500, var_id){
  DF <- map2_dfr(Dat.train, Dat.test, ~MC_fn(.x,.y, method=method, id=id, d=d, axes=axes, ntrees=ntrees, class=class, var_id=var_id), .id="fold")
  answer <- calc_misclassification(DF, class)
  #out <- list(DF=DF, answer=answer)
  answer
}

