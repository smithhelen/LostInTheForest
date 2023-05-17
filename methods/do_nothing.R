# Function for when we have pre-transformed the data but need information for uniques
donothing <- function(var, class) {
  if(is.numeric(var)) {
    var_levels = var
    list(output = var_levels, 
         #extra = list(var_levels = var_levels |> unique() |> as.factor() |> levels(), num_vars = 1, var_names=NULL))
         extra = list(var_levels = var_levels |> unique(), num_vars = 1, var_names=NULL))
    } else {     # map from variable levels to scores
    var_levels <- droplevels(var)
    if (nlevels(var_levels) < 2) {
      # if we only have one var_level we can't do anything
      return(list(output = rep(1, times=length(var)), 
                  extra = list(var_levels=levels(var_levels), num_vars = 1, var_names=NULL, 
                               score = data.frame(Var_Level = levels(var_levels), Rank = 1))))
    }
    ct <- table(Var_Level=var_levels, class=class)
    P <- ct/rowSums(ct)
    S <- cov.wt(P, wt = rowSums(ct))$cov
    pc1 <- eigen(S)$vectors[,1]   ## PCA of weighted covariance matrix of class probabilites
    score <- P %*% pc1 %>% as.data.frame() %>% rownames_to_column("Var_Level") %>% setNames(., c("Var_Level","PC1")) %>% mutate(Rank = rank(PC1, ties.method = "first"))
    output_factor <- data.frame(Var_Level = var_levels) %>% left_join(score, by="Var_Level") %>% pull(Rank)
    list(output = output_factor, 
         extra = list(score = score, var_levels = levels(var_levels), num_vars = 1, var_names=NULL))
    }
  }

# iterate over the variable columns and grab the output as a new data.frame, and store the absent level stuff for later 
prepare_training <- function(data, var_cols, class) {
  # pull out our variable columns and class
  var_cols <- dplyr::select(data, all_of(var_cols))
  classes   <- data %>% pull(class)
  # iterate over the variable columns, and convert
  prepped <- map(var_cols, donothing, class = classes)
  prepped_data <- bind_cols(data.frame(classes) %>% setNames(data %>% select(all_of(class)) %>% colnames), 
                            map_dfc(prepped, "output"))
  extra <- map(prepped, "extra")
  list(training = prepped_data, extra=extra)
}

# score absent levels as infinite
impute_ordinal <- function(var, extra) {
  if(is.numeric(var)) {
    var
  } else{
       score <- pluck(extra, "score")
       new_var_level_rank <- score %>% pull(Rank) %>% max() + 1
       ranked_var <- data.frame(Var_Level = var) %>%
         left_join(score, by="Var_Level") %>%
         replace_na(list(Rank = new_var_level_rank))
       ranked_var %>% pull(Rank)
  }
    
}

# iterate over the variable columns and use the extra info from before to remap the levels in the test data. 
prepare_test <- function(data, extra, id) {
  id <- data %>% pull(id)
  newdata_pred <- map2_dfc(data %>% select(any_of(names(extra))), extra, impute_ordinal) |> bind_cols(id=id)
}
