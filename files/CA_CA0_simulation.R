# load libraries
source("methods/libs_fns.R") 
library(randomForest)

# set seed
seed=123
set.seed(seed)



# calculate OOB and independent test set misclassification error rates using a range of parameters
CA0_vs_CA <- function(n=200, p=3, k=35, ntrees=500, ordered=TRUE){
  
  # generate data
  Data <- data.frame(Id = 1:n, Class = sample(1:p, n, TRUE), 
                     Var1 = sample(1:k, n, TRUE), 
                     Var2 = sample(1:k, n, TRUE), 
                     Var3 = sample(1:k, n, TRUE),
                     Var4 = sample(1:k, n, TRUE)) |> mutate(across(everything(), factor))
  
  # split data into train and test sets
  Dat.train <- Data  |> slice_sample(prop = 0.8) |> mutate(across(everything(), droplevels))
  Dat.test <- Data |> filter(!Id %in% Dat.train$Id) |> mutate(across(everything(), droplevels))
  
  # score training and test tests
  train.ca <- prepare_training_ca(Dat.train, starts_with("Var"), "Class")
  test.ca <- prepare_test_ca(Dat.test, train.ca$extra, "Id") 
  train.ca0 <- prepare_training_ca0(Dat.train, starts_with("Var"), "Class")
  test.ca0 <- prepare_test_ca0(Dat.test, train.ca0$extra, "Id")
  
  # create ranger model on training data
  mod.ca <- ranger(Class ~ ., data=train.ca$training, num.trees=ntrees, respect.unordered.factors = ordered)
  mod.ca0 <- ranger(Class ~ ., data=train.ca0$training, num.trees=ntrees, respect.unordered.factors = ordered)
  
  # test ranger model and calculate misclassification error rate
  preds.ca <- data.frame(Prediction = predict(mod.ca, data=test.ca, predict.all = FALSE)$predictions, Truth = Dat.test$Class) |> table()
  preds.ca0 <- data.frame(Prediction = predict(mod.ca0, data=test.ca0, predict.all = FALSE)$predictions, Truth = Dat.test$Class) |> table()
  
  # calculate misclassification rate
  misclass.ca <- 1-sum(diag(preds.ca))/sum(preds.ca)
  misclass.ca0 <- 1-sum(diag(preds.ca0))/sum(preds.ca0)
  
  # output results
  list(misclass.ca=misclass.ca, misclass.ca0=misclass.ca0, n=n, k=k, p=p)
}


# run over a range of values of n and k and plot
n <- list(10,50,100,150,200,400)
k <- list(1,5,10,35,50,100,150,200)
results <- map_dfr(n, function(x) {map(k, function(y) {CA0_vs_CA(n=x, k=y, p=3, ntrees=50, ordered=TRUE)})})
results |> pivot_longer(cols=c(OOB, misclass), values_to = "error", names_to = "method") |>
  ggplot(aes(x=k, y=error, colour=method, size=n)) + geom_point()

# repeat 99 times and average
#seed <- as.list(1:99)

results_ord <- map_dfr(1:99, function(z) map_dfr(n, function(x) map(k, ~OOB_check(n=x, k=., ordered=TRUE, method="ranger")))) |>
  group_by(n,k) |> summarise(OOB=mean(OOB), misclass=mean(misclass))
results_unord <- map_dfr(1:99, function(z) map_dfr(n, function(x) map(k, ~OOB_check(n=x, k=., ordered=FALSE, method="ranger")))) |>
  group_by(n,k) |> summarise(OOB=mean(OOB), misclass=mean(misclass))
results <- bind_rows("TRUE"=results_ord, "FALSE"=results_unord, .id = "Ordered")

#save(results, file="../Ranger_Data/results/OOB_results.Rdata") # run 8th July 2022
load("../Ranger_Data/results/OOB_results.Rdata")

# plot
results |> pivot_longer(cols=c(OOB, misclass), values_to = "error", names_to = "method") |>
  ggplot(aes(x=k, y=error, colour=method, size=n)) + geom_point() + 
  labs(y="Misclassification rate\n", x="\nNumber of factor levels", colour="Method of calculation", size="Sample size") + 
  scale_colour_discrete(labels = c("Independent test data", "Out-Of-Bag")) + 
  geom_hline(yintercept=2/3, linetype="dotted") + ylim(0,1) + 
  #  facet_wrap(~Ordered, labeller = labeller(Ordered=c(`FALSE`="respect.unordered.factors=FALSE",`TRUE`="respect.unordered.factors=TRUE"))) +
  #  facet_wrap(~Ordered, labeller = labeller(Ordered=c(`FALSE`="Alphabetical ordering",`TRUE`="Ordering based on class probabilities"))) +
  facet_wrap(~Ordered, labeller = labeller(Ordered=c(`FALSE`="Nominal encoding",`TRUE`="Target encoding"))) +
  theme(legend.text = element_text(size=11))

p <- results |> pivot_longer(cols=c(OOB, misclass), values_to = "error", names_to = "method") |> 
  ggplot(aes(x=k, y=error, colour=method, size=n, shape=method)) + 
  geom_point() + 
  #geom_hline(yintercept=1/2, linetype="dotted") +  
  geom_hline(yintercept=2/3, linetype="dotted") + 
  theme_bw(base_size = 14)+ 
  scale_y_continuous("Misclassification rate",expand=c(0,0),limits=c(0,1)) +
  scale_colour_manual(values=c("#d34728","#193d87"),labels = c("Independent test data", "Out-Of-Bag")) + 
  scale_shape_manual(values=c(16,18),labels = c("Independent test data", "Out-Of-Bag"))+
  labs(x="Number of factor levels", colour="Method of calculation", shape="Method of calculation", size="Sample size") + 
  facet_wrap(~Ordered, labeller = labeller(Ordered=c(`FALSE`="Target-agnostic encoding",`TRUE`="Target-based encoding"))) +
  scale_radius(limits = c(0, NA), range = c(1, 5)) + 
  guides(colour = guide_legend(order = 1, override.aes = list(size = 3)), 
         shape = guide_legend(order = 1), 
         size = guide_bins(direction = "vertical"))

pdf("OOB_plot_3classes.pdf", width=7, height=4.5)
p + theme_bw(base_size = 10.5, base_family = 'serif') +
  scale_radius(limits = c(0, NA), range = c(1.1, 4)) + 
  theme(axis.title.x = element_text(margin = ggplot2::margin(t=10)), axis.title.y = element_text(margin = ggplot2::margin(r=5)))
dev.off()

# What about when there are only two classes (p=2)?
results_ord <- map_dfr(1:99, function(z) map_dfr(n, function(x) map(k, ~OOB_check(n=x, k=., p=2, ordered=TRUE, method="ranger")))) |>
  group_by(n,k) |> summarise(OOB=mean(OOB), misclass=mean(misclass))
results_unord <- map_dfr(1:99, function(z) map_dfr(n, function(x) map(k, ~OOB_check(n=x, k=., p=2, ordered=FALSE, method="ranger")))) |>
  group_by(n,k) |> summarise(OOB=mean(OOB), misclass=mean(misclass))
results <- bind_rows("TRUE"=results_ord, "FALSE"=results_unord, .id = "Ordered")

#save(results, file="../Ranger_Data/results/OOB_results_2class.Rdata") # run 20th Sept 2023
load("../Ranger_Data/results/OOB_results_2class.Rdata")


results |> pivot_longer(cols=c(OOB, misclass), values_to = "error", names_to = "method") |>
  ggplot(aes(x=k, y=error, colour=method, size=n)) + geom_point() + 
  labs(y="Misclassification rate\n", x="\nNumber of factor levels", colour="Method of calculation", size="Sample size") + 
  scale_colour_discrete(labels = c("Independent test data", "Out-Of-Bag")) + 
  geom_hline(yintercept=1/2, linetype="dotted") + ylim(0,1) + 
  #  facet_wrap(~Ordered, labeller = labeller(Ordered=c(`FALSE`="respect.unordered.factors=FALSE",`TRUE`="respect.unordered.factors=TRUE"))) +
  #  facet_wrap(~Ordered, labeller = labeller(Ordered=c(`FALSE`="Alphabetical ordering",`TRUE`="Ordering based on class probabilities"))) +
  facet_wrap(~Ordered, labeller = labeller(Ordered=c(`FALSE`="Nominal encoding",`TRUE`="Target encoding"))) +
  theme(legend.text = element_text(size=11))+
  theme_bw()

p <- results |> pivot_longer(cols=c(OOB, misclass), values_to = "error", names_to = "method") |> 
  ggplot(aes(x=k, y=error, colour=method, size=n, shape=method)) + 
  geom_point() + 
  geom_hline(yintercept=1/2, linetype="dotted") +  
  #geom_hline(yintercept=2/3, linetype="dotted") + 
  theme_bw(base_size = 14)+ 
  scale_y_continuous("Misclassification rate",expand=c(0,0),limits=c(0,1)) +
  scale_colour_manual(values=c("#d34728","#193d87"),labels = c("Independent test data", "Out-Of-Bag")) + 
  scale_shape_manual(values=c(16,18),labels = c("Independent test data", "Out-Of-Bag"))+
  labs(x="Number of factor levels", colour="Method of calculation", shape="Method of calculation", size="Sample size") + 
  facet_wrap(~Ordered, labeller = labeller(Ordered=c(`FALSE`="Target-agnostic encoding",`TRUE`="Target-based encoding"))) +
  scale_radius(limits = c(0, NA), range = c(1, 5)) + 
  guides(colour = guide_legend(order = 1, override.aes = list(size = 3)), 
         shape = guide_legend(order = 1), 
         size = guide_bins(direction = "vertical"))

pdf("OOB_plot_2classes.pdf", width=7, height=4.5)
p + theme_bw(base_size = 10.5, base_family = 'serif') +
  scale_radius(limits = c(0, NA), range = c(1.1, 4)) + 
  theme(axis.title.x = element_text(margin = ggplot2::margin(t=10)), axis.title.y = element_text(margin = ggplot2::margin(r=5)))
dev.off()




# what about randomForest?
n <- list(50,100,150,200,400)
k <- list(5,10,20,30,40,50,53)
#results.rf <- map_dfr(n, function(x) map(k, ~OOB_check(n=x, k=., p=2, method="rf", ntrees=5)))
results.rf <- map_dfr(1:99, function(z) map_dfr(n, function(x) map(k, ~OOB_check(n=x, k=., p=2, method="rf", ntrees=500)))) |>
  group_by(n,k) |> summarise(OOB=mean(OOB), misclass=mean(misclass))
results.rf |> pivot_longer(cols=c(OOB, misclass), values_to = "error", names_to = "method") |>
  ggplot(aes(x=k, y=error, colour=method, size=n)) + geom_point() + 
  geom_hline(yintercept=1/2, linetype="dotted") + ylim(0,1) + 
  theme(legend.text = element_text(size=11))
#save(results.rf, file="../Ranger_Data/results/OOB_results.rf.Rdata") # run 13th June 2023
load("../Ranger_Data/results/OOB_results.rf.Rdata")

