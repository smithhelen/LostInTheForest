# Recipe for CA unbiased method for "Lost In The Forest"

source('methods/helpers.R')

# constructor function for our recipe step
step_ca_unbiased_new <- 
  function(terms, role, trained, k, objects, options, skip, id) {
    step(
      subclass = "ca_unbiased", 
      terms = terms,
      role = role,
      trained = trained,
      k = k,
      objects = objects,
      options = options,
      skip = skip,
      id = id
    )
  }

# user facing function for our recipe
step_ca_unbiased <- function(
    recipe, 
    ..., 
    role = "predictor", 
    trained = FALSE, 
    k = NULL, # defaults to number of classes - 1
    objects = NULL, # info from the training data
    skip = FALSE,
    options = list(),  # TODO: Add default options here (e.g. for PCO)
    id = rand_id("ca_unbiased")
) {
  
  add_step(
    recipe, 
    step_ca_unbiased_new(
      terms = enquos(...),
      trained = trained,
      role = role,
      k = k,
      objects = objects,
      options = options,
      skip = skip,
      id = id
    )
  )
}

# Prep step

# CA unbiased scoring function   
encode_ca_unbiased <- function(var, outcome, k) {
  var <- droplevels(var)
  if (nlevels(var) < 2) {
    # if we only have one level so we can't do anything
    return(NULL)
  }
  ct <- table(level=var, outcome=outcome)
  if(!length(k)) {
    k <- ncol(ct) - 1
  }
  P <- ct/rowSums(ct)
  S <- cov.wt(P, wt = rowSums(ct))$cov
  eigen_S <- eigen_decomp(S, symmetric = TRUE)   ## PCA of weighted covariance matrix of class probabilities

  # Restrict to a maximum of eigenvectors set by "k" (the number of axes) (default is NULL = ncol(ct)-1)
  nlambdas <- min(sum(eigen_S$values > epsilon), k)
  pc <- eigen_S$vectors
  X <- P %*% pc[, seq_len(nlambdas), drop=FALSE] |> as.data.frame() 
  objects <- list(levels = levels(var),
                  X=X)
  objects
}

prep.step_ca_unbiased <- function(x, training, info = NULL, ...) { 
  # select the columns we're going to prep
  col_names <- recipes_eval_select(x$terms, training, info)

  # select the outcome column
  outcome_name <- info |> filter(role == "outcome") |> pull(variable)
  if (length(outcome_name) != 1) {
    rlang::abort("One variable with role 'outcome' is required")
  }
  
  # checks
  check_type(training[, col_names], types = c("character", "factor"))
  check_type(training[, outcome_name], types = c("character", "factor"))
  
  # apply the CA step to each column
  objects <- purrr::map(
    training[, col_names], 
    \(var) encode_ca_unbiased(var = var, outcome = training |> pull(outcome_name), k = x$k)
  )
  
  ## Use the constructor function to return the updated object. 
  ## Note that `trained` is now set to TRUE
  step_ca_unbiased_new(
    terms = x$terms, 
    trained = TRUE,
    role = x$role, 
    k = x$k,
    objects = objects,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

# Bake step: take the scores and apply them as needed to the columns
apply_unbiased_to_column <- function(var, encoding) {
  
  new_level_to_ca0 <- function(new.var_level, X){
    newX <- matrix(NA, nrow = 1, ncol = ncol(X)) |> as.data.frame()
    colnames(newX) <- colnames(X)
    new_var_level_score <- data.frame(level = new.var_level, newX)
    new_var_level_score
  }
  
  # create the scoring matrix from the levels of var, and score new things as zero
  var <- droplevels(var) # ignore levels we don't have in these data
  
  # which levels are new, and which are the usual
  new_levels <- setdiff(levels(var), encoding$levels)
  new_scores <- map(new_levels, ~new_level_to_ca0(new.var_level = {.}, X = encoding$X)) |> list_rbind()
  var_level_score <- bind_rows(data.frame(encoding$X) |> rownames_to_column("level"), new_scores)
  new_cols <- data.frame(level = var) |> left_join(var_level_score, by = "level") |> select(-level)
  
  # add noise to imputed zero scores
  r <- min(abs(new_cols[new_cols != 0]), na.rm=TRUE)/100 
  new_cols <- new_cols |> rowwise() |> mutate(across(everything(), \(x) replace_na(x, runif(n=1, min=-r, max=r)))) |> ungroup()
  new_cols
}

bake.step_ca_unbiased <- function(object, new_data, ...) {
  col_names <- names(object$objects)
  check_new_data(col_names, object, new_data)
  
  # generate some new names
  new_names <- imap(object$objects, \(x, nm) { paste(nm, "ca0", seq_along(colnames(x$X)), sep="_") })
  new_tbl <- tibble::new_tibble(x = list(), nrow=nrow(new_data))
  
  # iterate over and generate new columns
  for (col_name in col_names) {
    i_col <- new_data[[col_name]]
    i_obj <- object$objects[[col_name]]
    if (!is.null(i_obj)) { 
      new_cols <- apply_unbiased_to_column(var = i_col, encoding = i_obj)
      new_col_names <- new_names[[col_name]]
      colnames(new_cols) <- new_col_names
      new_tbl[new_col_names] <- new_cols
    }
  }
  
  # check the new names and produce final dataset
  new_tbl <- check_name(new_tbl, new_data, object, names(new_tbl))
  new_data <- bind_cols(new_data, new_tbl)
  new_data <- dplyr::select(new_data, -dplyr::all_of(col_names))
  new_data
}

