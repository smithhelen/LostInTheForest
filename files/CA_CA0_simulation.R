# load libraries
library(tidyverse)
library(stringr)

# set seed
seed=1
set.seed(seed)

class <- function(level, p) {
  case_match(level, 
             "a" ~ "A",
             "b" ~ "B",
             "c" ~ if_else(rbinom(n=1,size=1,p=p)==1, "A","B"))
}

calc_accuracy <- function(prop.absent, p){
  level = c(rep("a", 50*(1-prop.absent)),
            rep("b", 50*(1-prop.absent)),
            rep("c", 100*prop.absent))
  
  dat = data.frame(level = level) |> 
    rowwise() |> mutate(
      pred.ca = class(level, 1),
      pred.ca0 = class(level, 0.5),
      class = class(level, p)) |> 
    mutate(accuracy.ca = ifelse(pred.ca == class, 1, 0),
           accuracy.ca0 = ifelse(pred.ca0 == class, 1, 0))

  accuracy = dat |> select(starts_with("accuracy")) |> colSums()
  
  accuracy.class = dat |> group_by(class) |> summarise(ca = sum(accuracy.ca)/n(),
                                                       ca0 = sum(accuracy.ca0)/n())
  
  out = accuracy.class |> bind_rows(data.frame(class="overall",
                                               ca=accuracy[1]/100,
                                               ca0=accuracy[2]/100))
  
  out
}

# repeat 99 times and average
accuracy <- function(prop.absent, p){
  map(1:200, ~calc_accuracy(prop.absent, p)) |> list_rbind() |> group_by(class) |> summarise(ca=mean(ca), ca0=mean(ca0))
}

# run over a range of values of prop.absent (proportion of absent levels)
prop.absent <- seq(0,1,0.1)

# run over a range of levels of association of "c" with "A" vs "B"
# p is the probability of "c" being associated with "A"
p <- c(0.2, 0.5, 0.8)

x <- expand.grid(prop.absent, p)
names <- x |> mutate(names = paste(Var1,Var2, sep = "_")) |> pull(names)

results <- map2(x$Var1, x$Var2, accuracy)
names(results) <- names

# plot
results |> list_rbind(names_to ="prop.absent_p") |> 
  rowwise() |> 
  mutate(prop.absent = str_split_i(prop.absent_p, "_", 1),
         p = str_split_i(prop.absent_p, "_", 2), .keep="unused", .before=1) |> 
  pivot_longer(cols=c("ca", "ca0"), values_to = "accuracy", names_to = "method") |>
  mutate(shape = case_match(class, c("A","B") ~ 19, .default = 4),
         stroke = case_match(class, c("A","B") ~ 1, .default = 1.5)) |> 
  ggplot(aes(x=prop.absent, y=accuracy, colour=class, shape=I(shape), stroke=I(stroke))) + 
  geom_point(size=3) +
  facet_grid(p~method, labeller = labeller(method=c(ca="CA", ca0="CA-unbiased"),
                                           p=c(`0.2`="Prob(absent level is A)=0.2", `0.5`="Prob(absent level is A)=0.5", `0.8`="Prob(absent level is A)=0.8"))) +
  scale_colour_manual(values=c("#d34728","#1463b5","#367d23"), labels=c("Class A", "Class B", "Overall")) + 
  scale_shape_identity(breaks=c(19,19,4), guide="legend", labels=c("Class A", "Class B", "Overall")) + 
  guides(shape=guide_legend(override.aes = list(stroke=c(1,1,1.5)))) +
  labs(title="Effect of absent levels on prediction accuracy",
       y="Classification success rate\n", 
       x="\nProportion of absent levels", 
       colour="Class accuracy", shape="Class accuracy") + 
  theme_bw(base_size = 12, base_family = 'serif')

