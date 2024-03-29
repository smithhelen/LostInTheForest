# Recipe for PCO method for "Lost In The Forest"

source('methods/helpers.R')

# constructor function for our recipe step
step_pco_new <- 
  function(terms, role, trained, distances, m, mp, objects, options, skip, id) {
    step(
      subclass = "pco", 
      terms = terms,
      role = role,
      trained = trained,
      distances = distances,
      m = m,
      mp = mp,
      objects = objects,
      options = options,
      skip = skip,
      id = id
    )
  }

# user facing function for our recipe
step_pco <- function(
    recipe, 
    ..., 
    role = "predictor", 
    trained = FALSE,
    distances,
    m = NULL,
    mp = 100,
    objects = NULL, # encoding info from our training data
    skip = FALSE,
    options = list(),
    id = rand_id("pco")
) {
  
  if (TRUE) { 
    m <- as.integer(m)
  }
  add_step(
    recipe, 
    step_pco_new(
      terms = enquos(...),
      trained = trained,
      role = role,
      distances = distances,
      m = m,
      mp = mp,
      objects = objects,
      options = options,
      skip = skip,
      id = id
    )
  )
}

# Prep step

# PCO scoring function
encode_pco <- function(var, distance, m, mp) {
  var <- droplevels(var)
  d.train <- distance[levels(var),levels(var),drop=FALSE]
  A.train <- -0.5 * d.train^2
  B.train <- dbl_center(A.train)
  eigen_B <- eigen_decomp(B.train, symmetric=TRUE)
  
  # use only non-zero eigenvalues
  nlambdas <- sum(eigen_B$values > epsilon)
  if (nlambdas == 0) {
    # No non-zero eigenvectors
    return(NULL)
  }
  
  # Restrict to a maximum of eigenvectors set by "m" or "mp (propG)" (default is mp=100% variation)
  lambdas_B <- filter_eigenvalues(eigen_B$values[seq_len(nlambdas)], m=m, mp=mp) # restrict by axes or mp
  
  # Scale eigenvectors
  Qo <- eigen_B$vectors
  Q <- sweep(Qo[, seq_along(lambdas_B), drop=FALSE], 2, sqrt(abs(lambdas_B)), "*")
  objects <- list(levels = levels(var),
                  d=distance,
                  Q=Q,
                  diag.B.train = diag(B.train),
                  lambdas_B=lambdas_B,
                  propG = cumsum(eigen_B$values)/sum(eigen_B$values)*100)
  objects
}

prep.step_pco <- function(x, training, info = NULL, ...) {
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
  if (!is.list(x$distances)) {
    rlang::abort("distances should be a list object of matrices")
  }
  if (!all(col_names %in% names(x$distances))) {
    rlang::abort("distances should have entries for all columns, named after the column")
  }
  if (!all(map_lgl(x$distances, ~ any(class(.) %in% c('dist', 'matrix'))))) {
    rlang::abort("distances should be of class 'dist' or 'matrix'")
  }
  # convert distances to matrices
  x$distances <- map(x$distances, as.matrix)
  
  # apply the PCO step to each column
  objects <- purrr::map2(
    training[, col_names],
    x$distances[col_names],
    \(var, dist) encode_pco(var=var, distance = dist, m = x$m, mp = x$mp)
  )
  
  ## Use the constructor function to return the updated object. 
  ## Note that `trained` is now set to TRUE
  step_pco_new(
    terms = x$terms, 
    trained = TRUE,
    role = x$role, 
    distances = x$distances,
    m = x$m,
    mp = x$mp,
    objects = objects,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

# Bake step: take the scores and apply them as needed to the columns
apply_pco_to_column <- function(var, encoding) {
  
  # Gower's transformation of new observations into PCO space
  new_level_to_pco <- function(new.var_level, d, diag.B.train, Q, lambdas_B) {
    d.new <- d[new.var_level, names(diag.B.train)]
    d.gower <- diag.B.train - (d.new ^ 2)
    newQ <- d.gower %*% Q / (2 * lambdas_B)
    colnames(newQ) = colnames(Q)
    new_var_level_score <- data.frame(level = new.var_level, newQ)
    new_var_level_score
  }
  
  var <- droplevels(var) # ignore levels we don't have in these data
  
  # which levels are new, and which are the usual
  new_levels <- setdiff(levels(var), encoding$levels)
  new_scores <- map(new_levels, ~new_level_to_pco(new.var_level = {.},
                                                  d = encoding$d,
                                                  diag.B.train = encoding$diag.B.train,
                                                  Q = encoding$Q, 
                                                  lambdas_B = encoding$lambdas_B)) |>
    list_rbind()
  
  var_level_score <- bind_rows(data.frame(encoding$Q) %>% rownames_to_column("level"), new_scores)
  new_cols <- data.frame(level = var) |> left_join(var_level_score, by="level") |> select(-level)
  new_cols
}

bake.step_pco <- function(object, new_data, ...) {
  col_names <- names(object$objects)
  check_new_data(col_names, object, new_data)
  new_names <- imap(object$objects, \(x, nm) { paste(nm, "pco", seq_along(x$lambdas_B), sep="_") })
  new_tbl <- tibble::new_tibble(x = list(), nrow=nrow(new_data))
  
  # iterate over and generate new columns
  for (col_name in col_names) {
    i_col <- new_data[[col_name]]
    i_obj <- object$objects[[col_name]]
    if (!is.null(i_obj)) { # only if we need to include this column...
      new_cols <- apply_pco_to_column(var = i_col, encoding = i_obj)
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

print.step_pco <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "PCO transformation on "
    
    print_step(
      # Names after prep:
      tr_obj = names(x$objects),
      # Names before prep (could be selectors)
      untr_obj = x$terms,
      # Has it been prepped? 
      trained = x$trained,
      # What does this step do?
      title = title,
      # An estimate of how many characters to print on a line: 
      width = width
    )
    invisible(x)
  }

format_pco <- function(x) {
  if (is.null(x)) {
    # consistency
    tibble(
      value = character(),
      axis = integer(),
      pco = numeric(),
      prop_var = numeric()
    )
  } else {
    prop_var <- data.frame(axis = seq_along(x$propG), prop_var = x$propG)
    x$Q |> as.data.frame() |>
      tibble::rownames_to_column("value") |>
      pivot_longer(-value, names_to="axis", values_to="pco", names_prefix="V", names_transform = as.integer) |>
      left_join(prop_var, by='axis')
  }
}

tidy.step_pco <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$objects) == 0) {
      # We need to create consistent output when no variables were selected
      res <- tibble(
        term = character(),
        value = character(),
        axis = integer(),
        prop_var = numeric(),
        pco = numeric()
      )
    } else {
      res <- map(x$objects, format_pco) |> list_rbind(names_to = "term")
    }
  } else {
    term_names <- sel2char(x$terms)
    res <-
      tibble(
        term = term_names,
        value = rlang::na_chr,
        axis = rlang::na_int,
        prop_var = rlang::na_dbl,
        pco = rlang::na_dbl
      )
  }
  # Always return the step id: 
  res$id <- x$id
  res
}
