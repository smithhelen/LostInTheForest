#### Prepare test data and training data for ca method ####

# Function to map from variable levels to scores
standard_ca_map <- function(var, class) {
  var_levels <- droplevels(var)
  if (nlevels(var_levels) < 2) {
    # if we only have one var_level we can't do anything
    return(list(output = var, 
                extra = list(var_levels=levels(var_levels), num_vars = 1, var_names=NULL, 
                             score = levels(var_levels) %>% as.data.frame() %>% set_names("Var_Level") %>% mutate(Rank = as.numeric(rank(.))))))
  }
  ct <- table(Var_Level=var_levels,Class=class)
  P <- ct/rowSums(ct)
  S <- cov.wt(P, wt = rowSums(ct))$cov
  pc1 <- eigen(S)$vectors[,1]   ## PCA of weighted covariance matrix of class probabilites
  score <- P %*% pc1 %>% as.data.frame() %>% rownames_to_column("Var_Level") %>% setNames(., c("Var_Level","PC1"))
  score2 <- score %>% mutate(Rank = as.numeric(rank(PC1, ties.method = "first"))) %>% select(Var_Level, Rank)
  var_factor <- factor(var, levels=(score %>% arrange(PC1) %>% pull(Var_Level)), ordered = TRUE, exclude = NULL)
  list(output = var_factor, extra = list(var_levels=levels(var_levels), num_vars = 1, var_names=NULL, score = score2))
}

# iterate over the variable columns and grab the output as a new data.frame for ca, and store the absent level stuff for later 
prepare_training_ca <- function(data, var_cols, class) {
  # pull out our variable columns and class
  var_cols <- dplyr::select(data,{{var_cols}})
  classes   <- data %>% pull({{class}})
  # iterate over the variable columns, and convert
  prepped <- map(var_cols, standard_ca_map, class = classes)
  prepped_data <- bind_cols(class = classes, map_dfc(prepped, "output"))
  extra <- map(prepped, "extra")
  list(training = prepped_data,
       extra=extra)
}

# ranger does all the work for us 
prepare_test_ca <- function(data) {
  data
}

# score absent levels as infinite
impute_ca <- function(var, extra) {
  score <- pluck(extra, "score")
  ranked_var <- data.frame(Var_Level = var) %>%
    left_join(score, by="Var_Level") %>%
    replace_na(list(Rank = Inf))
  ranked_var %>% pull(Rank)
}

# to be used in ca_testing.R
prepare_test_ca_reimpl <- function(data, extra, id) {
  id <- data %>% pull({{id}})
  newdata_pred <- map2_dfc(data %>% select(any_of(names(extra))), extra, impute_ca)
  newdata_pred <- bind_cols(id=id, newdata_pred)
}
