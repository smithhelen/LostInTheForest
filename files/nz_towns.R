# Towns

library(tidyverse)
library(stringr)
library(stringdist)
library(seqinr) #for col2alpha
source("methods/helpers.R") 

pco <- function(d){
  A <- -0.5 * d^2
  B <- dbl_center(A)
  eigen_B <- eigen_decomp(B, symmetric=TRUE)
  lambdas_B <- eigen_B$values[1:2]
  Qo <- eigen_B$vectors
  Q <- sweep(Qo[, seq_along(lambdas_B), drop=FALSE], 2, sqrt(abs(lambdas_B)), "*")
  score <- left_join(towns, data.frame(Q) |> rownames_to_column("city"), by = "city")
  score
}

#############################################################################
# nz towns
towns <- read_csv("data/nz.csv") |> filter(city != "Waitangi")

# calculate distance matrices based on name, population, and location
d.name <- stringdistmatrix(towns$city, towns$city, method = "lv", useNames = "names") 
colnames(d.name) <- towns$city
rownames(d.name) <- towns$city

d.pop <- towns |> select("population") |> dist(diag=TRUE, upper=TRUE) |> as.matrix() 
colnames(d.pop) <- towns$city
rownames(d.pop) <- towns$city

d.loc <- towns |> select(c("lat","lng")) |> dist(diag=TRUE, upper=TRUE) |> as.matrix()
colnames(d.loc) <- towns$city
rownames(d.loc) <- towns$city

# perform pco analysis
pco.name <- pco(d.name) |> rename(PCO1 = V1, PCO2 = V2)
pco.pop <- pco(d.pop) |> rename(PCO1 = V1, PCO2 = V2)
pco.loc <- pco(d.loc) |> rename(PCO1 = V1, PCO2 = V2)


# plot 1d and 2d
pco.plot <- list(pco.name, pco.pop, pco.loc) |> 
  reduce(right_join, by=c("city","lat","lng","population"), suffix = c(".name",".pop")) |> 
  rename(PCO1.loc = PCO1, PCO2.loc = PCO2) |> 
  pivot_longer(cols = c(starts_with("PCO1"), starts_with("PCO2")), 
               names_to = c(".value", "distance"), 
               names_sep = "\\.") |> 
  mutate(distance = factor(distance, levels=c("name","pop","loc")))

pco.plot |> ggplot(aes(x=-PCO2, y=PCO1, group=distance, label=city)) + 
  geom_text(size=2) +
  theme(#aspect.ratio = 1,
        panel.background = element_blank(),
        legend.position = "none") +
  facet_wrap(~distance, nrow = 1, scales="free",
             labeller = labeller(distance=c(name = "Distance from names",
                                            pop = "Distance from population",
                                            loc = "Distance from latitude and longitude"))) 



###################################################################################################)
