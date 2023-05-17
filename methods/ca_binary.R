#### Prepare test data and training data for ca_binary method ####

# load libraries
library(varhandle) # for to.dummy()

# iterate over the variable columns and grab the output as a new data.frame for ca, and store the absent level stuff for later 
binary_ca_map <- function(var, var_name) {
  var_levels <- droplevels(var)
  var_binary <- data.frame(to.dummy(var_levels, var_name))
  list(output = var_binary, 
       extra = list(var_levels=levels(var_levels), num_vars=1, var_names=names(var_binary), names = var_name))
}

prepare_training_ca_binary <- function(data, var_cols, class) {
  # pull out the variable columns and class
  var_cols <- dplyr::select(data, all_of(var_cols))
  classes   <- data %>% pull(class)
  # iterate over the variable columns, and convert
  prepped <- map2(var_cols, names(var_cols), binary_ca_map)
  prepped_data <- bind_cols(data.frame(classes) %>% setNames(data %>% select(all_of(class)) %>% colnames), 
                            map_dfc(prepped, "output"))
  extra <- map(prepped, "extra")
  list(training = prepped_data,
       extra=extra)
}

# score absent levels as zero
impute_binary <- function(var, extra) {
  var_name <- pluck(extra, "names")
  var_levels <- pluck(extra, "var_levels") # these are the levels in the training set for the predictor variable
  train_vars <- extra$var_names # these are the names of the new training binary variables. The will differ from var_levels if there are symbols or spaces!
  var_binary <- data.frame(to.dummy(var, var_name)) 
  test_vars <- var_binary %>% select(any_of(train_vars))
  missing_vars <- setdiff(var_binary %>% colnames(), train_vars)
  empty_cols <- data.frame(matrix(0, nrow=nrow(test_vars),ncol=length(missing_vars)))
  colnames(empty_cols) <- missing_vars
  test_binary <- test_vars %>% bind_cols(empty_cols)
  test_binary
}

# iterate over the variable columns and use the extra info from before to remap the levels in the test data
prepare_test_ca_binary <- function(data, list_of_extras, id) {
  id <- data %>% pull({{id}})
  test_data <- data %>% select(any_of(names(list_of_extras)))
  newdata_pred <- map2_dfc(test_data, list_of_extras, impute_binary)
  newdata_pred <- bind_cols(id=id, newdata_pred)
  newdata_pred
}

