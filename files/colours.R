# Colour

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
  score <- left_join(colours, data.frame(Q) |> rownames_to_column("Name"), by = "Name")
  score
}

#############################################################################
# r colours
colours <- data.frame(Name=colors()) |> rowwise() |> 
  mutate(Hex=col2alpha(Name),
         R=col2rgb(Name)[1],
         G=col2rgb(Name)[2],
         B=col2rgb(Name)[3])

# check for duplicated names and hex values - both spellings of grey and gray are there
colours$Name |> table() |> as.data.frame() |> filter(Freq>1)
colours$Hex |> table() |> as.data.frame() |> filter(Freq>1)

colours <- colours |> distinct(Hex, .keep_all = TRUE)

# calculate distance matrices based on name, hex name, and rgb values
d.name <- stringdistmatrix(colours$Name, colours$Name, method = "lv", useNames = "names") 
colnames(d.name) <- colours$Name
rownames(d.name) <- colours$Name

d.hex <- stringdistmatrix(colours$Hex, colours$Hex, method = "lv", useNames = "names") 
colnames(d.hex) <- colours$Name
rownames(d.hex) <- colours$Name

d.rgb <- colours |> select(c("R","G","B")) |> dist(diag=TRUE, upper=TRUE) |> as.matrix()
colnames(d.rgb) <- colours$Name
rownames(d.rgb) <- colours$Name

# perform pco analysis
pco.name <- pco(d.name) |> rename(PCO1 = V1, PCO2 = V2)
pco.hex <- pco(d.hex) |> rename(PCO1 = V1, PCO2 = V2)
pco.rgb <- pco(d.rgb) |> rename(PCO1 = V1, PCO2 = V2)


# plot 1d and 2d
pco.plot <- list(pco.name, pco.hex, pco.rgb) |> 
  reduce(right_join, by=c("Name","Hex","R","G","B"), suffix = c(".name",".hex")) |> 
  rename(PCO1.rgb = PCO1, PCO2.rgb = PCO2) |> 
  pivot_longer(cols = c(starts_with("PCO1"), starts_with("PCO2")), 
               names_to = c(".value", "distance"), 
               names_sep = "\\.") |> 
  mutate(distance = factor(distance, levels=c("name","hex","rgb")))

pco.plot |> ggplot(aes(x=PCO1, y=0, group=distance, colour=Name)) + 
  geom_point(size=2, pch=19) +
  scale_color_identity() +
  theme(panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
      legend.position = "none") +
  facet_wrap(~distance, nrow = 3, scales="free",
             labeller = labeller(distance=c(name = "Distance from colour names",
                                            hex = "Distance from Hex values",
                                            rgb = "Distance from RGB values"))) 

pco.plot |> ggplot(aes(x=PCO1, y=PCO2, group=distance, colour=Name)) + 
  geom_point(size=2, pch=19) +
  scale_color_identity() +
  theme(aspect.ratio = 1,
        panel.background = element_blank(),
        legend.position = "none") +
  facet_wrap(~distance, nrow = 1, scales="free",
             labeller = labeller(distance=c(name = "Distance from colour names",
                                            hex = "Distance from Hex values",
                                            rgb = "Distance from RGB values"))) 

###################################################################################################)
