#### Prepare test data and training data for ca_unbiased method ####

# Function to map from variable levels to ranks.

# Output is both the score information for the variable (i.e. a vector of the same length as the input vector) and
# the level mapping data.frame (mapping from var_level to score)
factor_to_ordinal <- function(var, class) {
  var_levels <- droplevels(var)
  ct <- table(Var_Level=var_levels,Class=class)
  # add zero row - can't add zeros to ct as P will complain. So add 1/num.classes to P (equal probabilities across classes)
  new <- matrix(rep((1/nlevels(class)),times=nlevels(class)),nrow=1, ncol=nlevels(class), dimnames = list(Var_Level="new",Class=colnames(ct)))
  P <- rbind(ct/rowSums(ct),new)
  S <- cov.wt(P, wt = c(rowSums(ct),0))$cov
  pc1 <- eigen(S)$vectors[,1]   ## PCA of weighted covariance matrix of class probabilites 
  score <- P %*% pc1 %>% as.data.frame() %>% rownames_to_column("Var_Level") %>% setNames(., c("Var_Level","PC1")) %>% mutate(Rank = rank(PC1, ties.method = "first"))
  output_factor <- data.frame(Var_Level = var_levels) %>% left_join(score, by="Var_Level") %>% pull(Rank)
  list(output = output_factor,
       extra = list(score = score, var_levels = levels(var_levels), num_vars=1, var_names=NULL))
}

# iterate over the variable columns and grab the output as a new data.frame to send into ca, and store the absent level stuff for later
prepare_training_ca0 <- function(data, var_cols, class) {
  # pull out our var_cols and class
  var_cols <- dplyr::select(data,{{var_cols}})
  classes   <- data %>% pull({{class}})
  # iterate over the var columns, and convert
  prepped <- map(var_cols, factor_to_ordinal, class = classes)
  prepped_data <- bind_cols(class = classes, map_dfc(prepped, "output"))
  extra <- map(prepped, "extra")
  list(training = prepped_data,
       extra = extra)
}

# given variable information and a level map, do the mapping. Absent levels are mapped to 'new'
impute_ordinal <- function(var, extra) {
  score <- pluck(extra, "score")
  new_var_level_rank <- score %>% filter(Var_Level == "new") %>% pull(Rank)
  ranked_var <- data.frame(Var_Level = var) %>%
    left_join(score, by="Var_Level") %>%
    replace_na(list(Rank = new_var_level_rank))
  ranked_var %>% pull(Rank)
}

# iterate over the variable columns and use the extra info from before to remap the levels in the test data. This is then ready for sending to ca
prepare_test_ca0 <- function(data, extra, id) {
  # first remap the variable levels to the appropriate ordinal level
  id <- data %>% pull({{id}})
  newdata_pred <- map2_dfc(data %>% select(any_of(names(extra))), extra, impute_ordinal)
  newdata_pred <- bind_cols(id=id, newdata_pred)
}
