# function to prepare the training and testing data and train a random forest (ranger) model
rf_model <- function(Dat.train, Dat.test, d=NULL, axes=2, method="raw", ntrees=500, id, class, var_id){
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
           train <- prepare_training_ca0(Dat.train, starts_with(var_id), class=class)
           test <- prepare_test_ca0(Dat.test, train$extra, id=id)
         },
         pco = {
           train <- prepare_training_pco(Dat.train, starts_with(var_id), class=class, d, axes=axes)
           test <- prepare_test_pco(Dat.test, train$extra, id=id)
         },
         raw = {
           train <- prepare_training(Dat.train, starts_with(var_id), class=class)
           test <- prepare_test(Dat.test, train$extra, id=id)
         }
  )
  classes <- train$training |> pull(class)
  ranger_mod <- if(method=="binary") {
    ranger(dependent.variable.name = class, data = train$training, oob.error = TRUE, num.trees=ntrees, respect.unordered.factors = TRUE)
  } else {
    ranger(classes ~ ., data=train$training |> select(-any_of(class)), oob.error = TRUE, num.trees=ntrees, respect.unordered.factors = TRUE)
  }
  out <- list(train=train, test=test, class=class, ranger_mod=ranger_mod, id=id)
  out
}

