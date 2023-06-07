# load libraries
source("methods/libs_fns.R")

# define ca_unbiased function for OOB testing
factor_to_ordinal <- function(var, class) {
  levels <- droplevels(var)
  ct <- table(Var=levels,Class=class)
  # add zero row - can't add zeros to ct as P will complain. So add 1/num.classes to P (equal probabilities across classes)
  new_row <- matrix(rep((1/nlevels(class)),times=nlevels(class)),nrow=1, ncol=nlevels(class), dimnames = list(Level="new",Class=colnames(ct)))
  P <- rbind(ct/rowSums(ct),new_row)
  S <- cov.wt(P, wt = c(rowSums(ct),0))$cov
  pc1 <- prcomp(S, rank. = 1)$rotation
  score <- P %*% pc1 %>% as.data.frame() %>% rownames_to_column("Level") %>% mutate(Rank = rank(PC1, ties.method = "first"))
  output_factor <- data.frame(Level = levels) %>% left_join(score, by="Level") %>% pull(Rank)
  list(output = output_factor,
       extra = list(score = score, levels = levels(levels), num_vars=1, var_names=NULL))
}

prepare_training_ca0 <- function(data, genes, class) {
  gene_cols <- dplyr::select(data,{{genes}})
  classes   <- data %>% pull({{class}})
  prepped <- map(gene_cols, factor_to_ordinal, class = classes)
  prepped_data <- bind_cols(Class = classes, map_dfc(prepped, "output"))
  extra <- map(prepped, "extra")
  list(training = prepped_data,
       extra = extra)
}

impute_ordinal <- function(gene, extra) {
  score <- pluck(extra, "score")
  new_level_rank <- score %>% filter(Level == "new") %>% pull(Rank)
  ranked_var <- data.frame(Level = gene) %>%
    left_join(score, by="Level") %>%
    replace_na(list(Rank = new_level_rank))
  ranked_var %>% pull(Rank)
}

prepare_test_ca0 <- function(data, extra) {
  Id     <- data %>% pull(Id)
  newdata_pred <- map2_dfc(data %>% select(any_of(names(extra))), extra, impute_ordinal)
  newdata_pred <- bind_cols(Id=Id, newdata_pred)
}

# calculate OOB and independent test set misclassification error rates using a range of parameters
OOB_check <- function(Data, n=200, k=35, seed=123, ordered=TRUE, method=ranger){
  # set seed
  set.seed(seed)
  
  # generate data
  Data <- data.frame(Id = 1:n, Class = sample(1:3,n,TRUE), 
                     Var1 = sample(1:k, n, TRUE), 
                     Var2 = sample(1:k, n, TRUE), 
                     Var3 = sample(1:k, n, TRUE),
                     Var4 = sample(1:k, n, TRUE)) %>% mutate(across(everything(), factor))
  
  # split data into train and test sets
  Dat.train <- Data  %>% slice_sample(prop = 0.8) %>% mutate(across(everything(), droplevels))
  Dat.test <- Data %>% filter(!Id %in% Dat.train$Id) %>% mutate(across(everything(), droplevels))
  
  switch(method,
         ranger = {
           # run ranger using default ranger to generate OOB error rate
           OOB <- ranger(Class ~ ., data=Data %>% select(-Id), respect.unordered.factors = ordered, oob.error = TRUE)$prediction.error
           
           # create ranger model on training data
           mod <- ranger(Class ~ ., data=Dat.train %>% select(-Id), respect.unordered.factors = ordered, oob.error = TRUE)

           # test ranger model and calculate misclassification error rate
           preds <- data.frame(Prediction = predict(mod, data=Dat.test, predict.all = FALSE)$predictions, Truth = Dat.test$Class) %>% table()
         },

         ca0 = {
           train <- prepare_training_ca0(Dat.train, starts_with("Var"), "Class")
           test <- prepare_test_ca0(Dat.test, train$extra)
           data <- prepare_training_ca0(Data, starts_with("Var"), "Class")
           
           mod <- ranger(Class ~ ., data=train$training, respect.unordered.factors = ordered, oob.error = TRUE)
           preds <- data.frame(Prediction = predict(mod, data=test, predict.all = FALSE)$predictions, Truth = Dat.test$Class) %>% table()
           
           OOB <- ranger(Class ~ ., data=data$training, respect.unordered.factors = ordered, oob.error = TRUE)$prediction.error
           
         },

         binary = {
           # convert to binary
           Data.binary <- Data %>% select(Class) %>% bind_cols(Data %>% select(-c(Id, Class)) %>% imap_dfc(~ as_tibble(to.dummy(.x, .y))))
           Dat.train.binary <-  Dat.train %>% select(Class) %>% bind_cols(Dat.train %>% select(-c(Id, Class)) %>% imap_dfc(~ as_tibble(to.dummy(.x, .y))))
           Dat.test.binary <-  Dat.test %>% select(Class) %>% bind_cols(Dat.test %>% select(-c(Id, Class)) %>% imap_dfc(~ as_tibble(to.dummy(.x, .y))))
           missing_vars <- setdiff(Dat.train.binary %>% colnames(), Dat.test.binary %>% colnames())
           empty_cols <- data.frame(matrix(0, nrow=nrow(Dat.test.binary),ncol=length(missing_vars)))
           colnames(empty_cols) <- missing_vars
           Dat.test.binary <- Dat.test.binary %>% bind_cols(empty_cols)
           
           # run ranger using binary method to generate OOB error rate
           OOB <- ranger(dependent.variable.name = "Class", data=Data.binary, respect.unordered.factors = TRUE, oob.error = TRUE)$prediction.error

           # create ranger model on binary training data
           mod <- ranger(dependent.variable.name = "Class", data=Dat.train.binary, respect.unordered.factors = TRUE, oob.error = TRUE)
         
           # test ranger model and calculate misclassification error rate
           preds <- data.frame(Prediction = predict(mod, data=Dat.test.binary, predict.all = FALSE)$predictions, Truth = Dat.test.binary$Class) %>% table()
         })
  
  misclass <- 1-sum(diag(preds))/sum(preds)
  
  # output results
  list(OOB=OOB, misclass=misclass, n=n, k=k)
}


# run over a range of values of n and k and plot
n <- list(10,50,100,150,200,400)
k <- list(1,5,10,35,50,100,150,200)
results <- map_dfr(n, function(x) map(k, ~OOB_check(n=x, k=., ordered=TRUE, method="ranger")))
results %>% pivot_longer(cols=c(OOB, misclass), values_to = "error", names_to = "method") %>%
  ggplot(aes(x=k, y=error, colour=method, size=n)) + geom_point()

# repeat 99 times and average
seed <- as.list(1:99)

results_ord <- map_dfr(seed, function(z) map_dfr(n, function(x) map(k, ~OOB_check(n=x, k=., seed=z, ordered=TRUE, method="ranger")))) %>%
  group_by(n,k) %>% summarise(OOB=mean(OOB), misclass=mean(misclass))

results_unord <- map_dfr(seed, function(z) map_dfr(n, function(x) map(k, ~OOB_check(n=x, k=., seed=z, ordered=FALSE, method="ranger")))) %>%
  group_by(n,k) %>% summarise(OOB=mean(OOB), misclass=mean(misclass))

results <- bind_rows("TRUE"=results_ord, "FALSE"=results_unord, .id = "Ordered")

#save(results, file="../Ranger_Data/results/OOB_results.Rdata") # run 8th July 2022
load("../Ranger_Data/results/OOB_results.Rdata")

#load("results/OOB_results_binary.Rdata") # the above repeated with method = binary - moved to Finished folder

# plot
results_ord %>% pivot_longer(cols=c(OOB, misclass), values_to = "error", names_to = "method") %>%
  ggplot(aes(x=k, y=error, colour=method, size=n)) + geom_point() + 
  labs(title="Average misclassification error rates for a range of sample sizes and number of factor levels using ordered=TRUE", 
       y="Misclassification rate", x="Number of factor levels", colour="Method", size="Sample size") + 
  scale_colour_discrete(labels = c("Independent test data", "Out-of-bag")) + 
  geom_hline(yintercept=2/3, linetype="dotted") + ylim(0,1)

results_unord %>% pivot_longer(cols=c(OOB, misclass), values_to = "error", names_to = "method") %>%
  ggplot(aes(x=k, y=error, colour=method, size=n)) + geom_point() + 
  labs(title="Average misclassification error rates for a range of sample sizes and number of factor levels using ordered=FALSE", 
       y="Misclassification rate", x="Number of factor levels", colour="Method", size="Sample size") + 
  scale_colour_discrete(labels = c("Independent test data", "Out-of-bag")) + 
  geom_hline(yintercept=2/3, linetype="dotted") + ylim(0,1)



results %>% pivot_longer(cols=c(OOB, misclass), values_to = "error", names_to = "method") %>%
  ggplot(aes(x=k, y=error, colour=method, size=n)) + geom_point() + 
  labs(y="Misclassification rate\n", x="\nNumber of factor levels", colour="Method of calculation", size="Sample size") + 
  scale_colour_discrete(labels = c("Independent test data", "Out-Of-Bag")) + 
  geom_hline(yintercept=2/3, linetype="dotted") + ylim(0,1) + 
#  facet_wrap(~Ordered, labeller = labeller(Ordered=c(`FALSE`="respect.unordered.factors=FALSE",`TRUE`="respect.unordered.factors=TRUE"))) +
  facet_wrap(~Ordered, labeller = labeller(Ordered=c(`FALSE`="Alphabetical ordering",`TRUE`="Ordering based on class probabilities"))) +
  theme(legend.text = element_text(size=11))

# What about for ca_unbiased method?
results_true <- map_dfr(n, function(x) map(k, ~OOB_check(n=x, k=., ordered=TRUE, method="ca0")))
results_true %>% pivot_longer(cols=c(OOB, misclass), values_to = "error", names_to = "method") %>%
  ggplot(aes(x=k, y=error, colour=method, size=n)) + geom_point()
results_false <- map_dfr(n, function(x) map(k, ~OOB_check(n=x, k=., ordered=FALSE, method="ca0")))
results_false %>% pivot_longer(cols=c(OOB, misclass), values_to = "error", names_to = "method") %>%
  ggplot(aes(x=k, y=error, colour=method, size=n)) + geom_point()
