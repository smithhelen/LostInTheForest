# Lost In The Forest analyses using recipes

# Load librarys and method
source("methods/libs_fns.R")
library(lme4)

# Load data
load("../CAP_data/data/list_of_distance_matrices.RData")
load("../CAP_data/data/cgMLST_dat.RData") # SACNZ cgMLST data set (jejuni and coli)
Dat_jc <- cgMLST %>% filter(Source != "Human") %>% droplevels() %>% mutate(across(everything(), factor)) 

# For smaller sample
#Dat_jc <- Dat_jc |> select(c(1:4, "Source"))
#list_of_distance_matrices <- list_of_distance_matrices[1:3]

set.seed(234)

# Split data into 10 folds
jc_folds <- vfold_cv(Dat_jc, v = 10)

# use a single axis for all methods
k=1 # single ca axis
m=1 # single pco axis

## Define model
rf_mod <- 
  rand_forest(trees = 500)  |>  
  set_engine("ranger", respect.unordered.factors = TRUE) |> 
  set_mode("classification")

## Define recipes
basic_recipe <- recipe(Source ~ ., data=Dat_jc)
ca_recipe <- basic_recipe |> step_ca(starts_with("CAMP"), k=k) |> update_role(LabID, new_role = "id")
#ca0_recipe <- basic_recipe |> step_ca_unbiased(starts_with("CAMP"), k=k) |> update_role(LabID, new_role = "id")
#pco_recipe <- basic_recipe |> step_pco(starts_with("CAMP"), distances = list_of_distance_matrices, m=m) |> update_role(LabID, new_role = "id")

## Workflow
ca_wf <- workflow() |> add_model(rf_mod) |> add_recipe(ca_recipe)
#ca0_wf <- workflow() |> add_model(rf_mod) |> add_recipe(ca0_recipe)
#pco_wf <- workflow() |> add_model(rf_mod) |> add_recipe(pco_recipe)

## Fit resamples  - ERROR HERE
ca_fit_rs <- fit_resamples(
  ca_wf,
  resamples = jc_folds,
  control = control_resamples(save_pred=TRUE)
)

## Collect metrics
collect_metrics(ca_fit_rs)


### Run on single training and testing set
set.seed(345)

# Split data into single training and test set
split <- initial_split(Dat_jc)
jc_train <- training(split)
jc_test  <- testing(split)

## Recipes - ca (with scores not ranks), ca0, and pco
ca_recipe <- recipe(Source ~ ., data=jc_train) |> step_ca(starts_with("CAMP"), k=k) |> update_role(LabID, new_role = "id")
#ca0_recipe <- recipe(Source ~ ., data=jc_train) |> step_ca_unbiased(starts_with("CAMP"), k=k) |> update_role(LabID, new_role = "id")
#pco_recipe <- recipe(Source ~ ., data=jc_train) |> step_pco(starts_with("CAMP"), distances = list_of_distance_matrices, m=m) |> update_role(LabID, new_role = "id")

## Prep step
prepped_ca_recipe <- ca_recipe |> prep()
#prepped_ca0_recipe <- ca0_recipe |> prep()
#prepped_pco_recipe <- pco_recipe |> prep()

## Juice training step (same as bake really)
ca_train <- prepped_ca_recipe |> juice() # same as 'prepped_ca_recipe |> bake(jc_train)'
#ca0_train <- prepped_ca0_recipe |> juice()
#pco_train <- prepped_pco_recipe |> juice()
#glimpse(ca_train)

## Bake testing step
ca_test <- prepped_ca_recipe |> bake(jc_test)
#ca0_test <- prepped_ca0_recipe |> bake(jc_test)
#pco_test <- prepped_pco_recipe |> bake(jc_test)

## Define model
rf_mod <- 
  rand_forest(trees = 500)  |>  
  set_engine("ranger", respect.unordered.factors = TRUE) |> 
  set_mode("classification")

## Model training
ca_rf <- rf_mod |> fit(Source ~ ., data = ca_train)
#ca0_rf <- rf_mod |> fit(Source ~ ., data = ca0_train)
#pco_rf <- rf_mod |> fit(Source ~ ., data = pco_train)

## Collect metrics
ca_rf |> predict(ca_test) |> bind_cols(ca_test |> select(!starts_with("CAMP"))) |> 
  metrics(truth = Source, estimate = .pred_class)



## Do my own cross-validation
df <- map(jc_folds$splits, \(x) {
  jc_train <- x |> training()
  jc_test <- x |> testing()
  
  ca_recipe <- recipe(Source ~ ., data=jc_train) |> step_ca(starts_with("CAMP"), k=k) |> update_role(LabID, new_role = "id")
  ca0_recipe <- recipe(Source ~ ., data=jc_train) |> step_ca_unbiased(starts_with("CAMP"), k=k) |> update_role(LabID, new_role = "id")
  pco_recipe <- recipe(Source ~ ., data=jc_train) |> step_pco(starts_with("CAMP"), distances = list_of_distance_matrices, m=m) |> update_role(LabID, new_role = "id")
  
  prepped_ca_recipe <- ca_recipe |> prep()
  prepped_ca0_recipe <- ca0_recipe |> prep()
  prepped_pco_recipe <- pco_recipe |> prep()
  
  ca_train <- prepped_ca_recipe |> juice() # same as 'prepped_ca_recipe |> bake(jc_train)'
  ca0_train <- prepped_ca0_recipe |> juice()
  pco_train <- prepped_pco_recipe |> juice()
  
  ca_test <- prepped_ca_recipe |> bake(jc_test)
  ca0_test <- prepped_ca0_recipe |> bake(jc_test)
  pco_test <- prepped_pco_recipe |> bake(jc_test)
  
  ca_rf <- rf_mod |> fit(Source ~ ., data = ca_train)
  ca0_rf <- rf_mod |> fit(Source ~ ., data = ca0_train)
  pco_rf <- rf_mod |> fit(Source ~ ., data = pco_train)
  
  ca_preds <- ca_rf |> predict(ca_test) |> bind_cols(ca_test |> select(!starts_with("CAMP"))) 
  ca0_preds <- ca0_rf |> predict(ca0_test) |> bind_cols(ca0_test |> select(!starts_with("CAMP"))) 
  pco_preds <- pco_rf |> predict(pco_test) |> bind_cols(pco_test |> select(!starts_with("CAMP"))) 
  
  list(ca=ca_preds, ca0=ca0_preds, pco=pco_preds) |> list_rbind(names_to="method")
}) |> list_rbind(names_to="fold")

df

lm.method.dat <- df |> group_by(method, fold) |> rename(trueSource = Source) |> 
  rename(prediction = .pred_class) |> 
  mutate(correct = trueSource == prediction) |> 
  summarise(miss = 1-sum(correct)/n()) |> 
  ungroup() 
lm.method <- lm.method.dat |> 
  lm(miss ~ method, data=_) |> 
  augment(interval="confidence", se_fit=TRUE, newdata = df |> select(method) |> unique())

lm.method.fold.dat <- df |> group_by(method, fold) |> rename(trueSource = Source) |> 
  rename(prediction = .pred_class) |> 
  mutate(correct = trueSource == prediction) |> 
  summarise(miss = 1-sum(correct)/n()) |> 
  ungroup() 
lm.method.fold <- lm.method.fold.dat |> 
  lm(miss ~ method + fold, data=_) |> 
  augment(interval="confidence", newdata = df |> select(method, fold) |> unique())
lm.method.fold

lm.method.fold.dat |> 
  lmer(miss ~ method + (1|fold), data=_) |> 
  summary()

lm.method.source.fold.dat <- df |> group_by(method, Source, fold) |> 
  rename(trueSource = Source) |> 
  rename(prediction = .pred_class) |> 
  mutate(correct = trueSource == prediction) |> 
  summarise(miss = 1-sum(correct)/n()) |> 
  ungroup()

lm.method.source.fold.dat |> 
  lmer(miss ~ method + trueSource + method:trueSource + (trueSource|fold), data=_) |> 
  summary()




method <- c("ca","ca0","pco")
results <- map(method, \(x) {
  # forest predictions
  dat <- df |> filter(method == x) |> 
    rename(trueSource = Source) |> 
    rename(prediction = .pred_class) |> 
    mutate(correct = trueSource == prediction) 
  
  # weights for each fold
  w <- dat |> group_by(fold) |> tally() |> pull(n)
  
  # misclassification rates of the folds
  lm.dat <- dat |> group_by(fold) |> summarise(miss = 1-sum(correct)/n()) 
  #lm <- lm.dat |> lm(miss ~ 1, weights = w, data=_) |> summary()
  lm <- lm.dat |> lm(miss ~ 1, data=_) |> summary()
  
  # weighted mean and standard error of the misclassification rates
  av <- coef(lm)[,"Estimate"]
  se <- coef(lm)[,"Std. Error"]
  
  # confusion matrix
  conf <- dat |> 
    group_by(fold, trueSource, prediction) |> 
    summarise(n=n()) |> 
    mutate(N=sum(n), p=n/sum(n)) |> 
    ungroup() |>
    mutate(w = as.numeric(paste(factor(fold, labels=w))), t_p = paste(trueSource, prediction, sep="_")) |> 
    split(~t_p)
  
  # weighted mean and standard error of the confusion matrices
  conf.lm <- map(conf, function(x) {lm(p~1, weights=w, data=x) |> summary()})
  conf.av <- map(conf.lm, function(x) coef(x)[,"Estimate"])
  conf.se <- map(conf.lm, function(x) coef(x)[,"Std. Error"])
  
  out <- list(misclass=data.frame(av=av, se=se), 
              conf=data.frame(conf.av=conf.av |> as.matrix(),
                              conf.se=conf.se |> as.matrix()))
  out
  })
names(results) <- method
results
results |> map("misclass") |> list_rbind(names_to = "method")
results |> map("conf") |> list_cbind()

lm.dat <- df |> rename(trueSource = Source) |> rename(prediction = .pred_class) |> mutate(misclass = trueSource != prediction) 

lm1 <- lm(misclass ~ method, data=lm.dat) |> summary()
lmer1 <- lmer(misclass ~ method + (method|fold), data=lm.dat) |> summary()
augment(lm1, interval="confidence")

lm(misclass ~ trueSource + method + trueSource:method, data=lm.dat) |> summary()
lmer(misclass ~ trueSource + method + trueSource:method + (trueSource:method|fold), data=lm.dat) |> summary()

lm(misclass ~ trueSource + method + trueSource:method + fold + fold:trueSource, data=lm.dat) |> summary()
lmer(misclass ~ trueSource + method + trueSource:method + (trueSource:method|fold), data=lm.dat) |> summary()
