# function to prepare the training and testing data
prep_data <- function(Dat.train, Dat.test, method="ca", d=NULL, axes=2, id, class, var_id){
  switch(method,
         ca = {
           train <- prepare_training_ca(Dat.train, starts_with(var_id), class=class)
           test <- prepare_test_ca(Dat.test, train$extra, id=id)
         },
         binary = {
           train <- prepare_training_binary(Dat.train, starts_with(var_id), class=class)
           test <- prepare_test_binary(Dat.test, train$extra, id=id)
         },
         ca0 = {
           train <- prepare_training_ca0(Dat.train, starts_with(var_id), class=class)
           test <- prepare_test_ca0(Dat.test, train$extra, id=id)
         },
         pco = {
           train <- prepare_training_pco(Dat.train, starts_with(var_id), class=class, d, axes=axes)
           test <- prepare_test_pco(Dat.test, train$extra, id=id)
         },
         similarity = {
           train <- prepare_training_similarity(Dat.train, starts_with(var_id), class=class, d)
           test <- prepare_test_similarity(Dat.test, train$extra, id=id)
         }
  )
  prepped <- list(train=train, test=test)
}

# function to prepare the training and testing data and train a random forest (ranger) model
rf_prep_and_model <- function(Dat.train, Dat.test, method="ca", d=NULL, axes=2, id, class, var_id, ntrees=500){
  prepped <- prep_data(Dat.train, Dat.test, method, d, axes, id, class, var_id)
  train <- prepped$train
  test <- prepped$test
  classes <- train$training |> pull(class)
  ranger_mod <- if(method=="binary") {
    ranger(dependent.variable.name = class, data = train$training, oob.error = TRUE, num.trees=ntrees, respect.unordered.factors = TRUE)
  } else {
    ranger(classes ~ ., data=train$training |> select(-any_of(class)), oob.error = TRUE, num.trees=ntrees, respect.unordered.factors = TRUE)
  }
  out <- list(train=train, test=test, class=class, ranger_mod=ranger_mod, id=id)
  out
}

# function to train a random forest (ranger) model and return extras for tree_predictions
rf_model <- function(prepped.data, method="ca", id, class, ntrees=500){
  train <- prepped.data$train
  test <- prepped.data$test
  classes <- train$training |> pull(class)
  ranger_mod <- if(method=="binary") {
    ranger(dependent.variable.name = class, data = train$training, oob.error = TRUE, num.trees=ntrees, respect.unordered.factors = TRUE)
  } else {
    ranger(classes ~ ., data=train$training |> select(-any_of(class)), oob.error = TRUE, num.trees=ntrees, respect.unordered.factors = TRUE)
  }
  out <- list(train=train, test=test, class=class, ranger_mod=ranger_mod, id=id)
  out
}
