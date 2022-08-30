#### Prepare test data and training data for ca_binary method ####

# load libraries
library(varhandle) # for to.dummy()

# iterate over the variable columns and grab the output as a new data.frame for ca, and store the absent level stuff for later 
binary_ca_map <- function(var, var_name) {
  var_levels <- droplevels(var)
  var_binary <- data.frame(to.dummy(var_levels, var_name))
  list(output = var_binary, 
       extra = list(var_levels=levels(var_levels), num_vars = 1, var_names=NULL, names = var_name))
}

prepare_training_ca_binary <- function(data, var_cols, class) {
  # pull out the variable columns and class
  var_cols <- dplyr::select(data,{{var_cols}})
  classes   <- data %>% pull({{class}})
  # iterate over the variable columns, and convert
  prepped <- map2(var_cols, names(var_cols), binary_ca_map)
  prepped_data <- bind_cols(class = classes, map_dfc(prepped, "output"))
  extra <- map(prepped, "extra")
  list(training = prepped_data,
       extra=extra)
}

# score absent levels as zero
impute_binary <- function(var, extra) {
  var_name <- pluck(extra, "names")
  var_levels <- pluck(extra, "var_levels")
  vars <- paste(var_name, var_levels, sep=".")
  var_binary <- data.frame(to.dummy(var, var_name)) %>% select(any_of(vars))
  missing_vars <- setdiff(vars, var_binary %>% colnames())
  empty_cols <- data.frame(matrix(0, nrow=nrow(var_binary),ncol=length(missing_vars)))
  colnames(empty_cols) <- missing_vars
  var_binary <- var_binary %>% bind_cols(empty_cols)
}

# iterate over the variable columns and use the extra info from before to remap the levels in the test data
prepare_test_ca_binary <- function(data, extra, id) {
  id <- data %>% pull({{id}})
  newdata_pred <- map2_dfc(data %>% select(any_of(names(extra))), extra, impute_binary)
  newdata_pred <- bind_cols(id=id, newdata_pred)
}

