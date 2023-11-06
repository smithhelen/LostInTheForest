# Colour

library(tidyverse)
library(stringr)
library(stringdist)
library(seqinr) #for col2alpha
library(DescTools)
library(farver)
source("methods/helpers.R") 

pco <- function(d){
  A <- -0.5 * d^2
  B <- dbl_center(A)
  eigen_B <- eigen_decomp(B, symmetric=TRUE)
  lambdas_B <- eigen_B$values[1:2]
  Qo <- eigen_B$vectors
  Q <- sweep(Qo[, seq_along(lambdas_B), drop=FALSE], 2, sqrt(abs(lambdas_B)), "*")
  score <- left_join(colours |> select(Name, Hex), data.frame(Q) |> rownames_to_column("Name"), by = "Name")
  score
}

#############################################################################
# r colours
# rgb (red/green/blue); hsv (hue/saturation/value).
colours <- data.frame(Name=colors()) |> rowwise() |> 
  mutate(Hex=col2alpha(Name),
         R=col2rgb(Name)[1],
         G=col2rgb(Name)[2],
         B=col2rgb(Name)[3]) |> 
  mutate(HSV.H=ColToHsv(Hex)[1],
         HSV.S=ColToHsv(Hex)[2],
         HSV.V=ColToHsv(Hex)[3]) |> 
  mutate(HSL.H=decode_colour(Name, to="hsl")[1],
         HSL.S=decode_colour(Name, to="hsl")[2],
         HSL.L=decode_colour(Name, to="hsl")[3]) |> ungroup()

# check for duplicated names and hex values - both spellings of grey and gray are there
colours$Name |> table() |> as.data.frame() |> filter(Freq>1)
colours$Hex |> table() |> as.data.frame() |> filter(Freq>1)

colours <- colours |> distinct(Hex, .keep_all = TRUE)

# calculate distance matrices based on name, rgb, and hsv values
d.name <- stringdistmatrix(colours$Name, colours$Name, method = "lv", useNames = "names") 
colnames(d.name) <- colours$Name
rownames(d.name) <- colours$Name

d.rgb <- colours |> select(c("R","G","B")) |> dist(diag=TRUE, upper=TRUE) |> as.matrix()
colnames(d.rgb) <- colours$Name
rownames(d.rgb) <- colours$Name

d.hsv <- colours |> select(c("HSV.H","HSV.S","HSV.V")) |> dist(diag=TRUE, upper=TRUE) |> as.matrix()
colnames(d.hsv) <- colours$Name
rownames(d.hsv) <- colours$Name

d.hsl <- colours |> select(c("HSL.H","HSL.S","HSL.L")) |> dist(diag=TRUE, upper=TRUE) |> as.matrix()
colnames(d.hsl) <- colours$Name
rownames(d.hsl) <- colours$Name

# perform pco analysis
pco.name <- pco(d.name) |> rename(PCO1.name = V1, PCO2.name = V2)
pco.rgb <- pco(d.rgb) |> rename(PCO1.rgb = V1, PCO2.rgb = V2)
pco.hsv <- pco(d.hsv) |> rename(PCO1.hsv = V1, PCO2.hsv = V2) |> mutate(PCO1.hsv = -PCO1.hsv)
pco.hsl <- pco(d.hsl) |> rename(PCO1.hsl = V1, PCO2.hsl = V2)


# plot 1d and 2d
pco.plot <- list(pco.name, pco.rgb, pco.hsv, pco.hsl) |> 
  reduce(left_join, by = c("Name","Hex")) |>  
  pivot_longer(cols = c(starts_with("PCO1"), starts_with("PCO2")), 
               names_to = c(".value", "distance"), 
               names_sep = "\\.") |> 
  mutate(distance = factor(distance, levels=c("name","rgb", "hsv", "hsl")))

pco.plot |> ggplot(aes(x=PCO1, y=0, group=distance, colour=Name)) + 
  geom_point(size=2, pch=19) +
  scale_color_identity() +
  theme(panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
      legend.position = "none") +
  facet_wrap(~distance, nrow = 4, scales="free",
             labeller = labeller(distance=c(name = "Distance from colour names",
                                            rgb = "Distance from RGB values",
                                            hsv = "Distance from HSV values",
                                            hsl = "Distance from HSL values"))) 

pco.plot |> ggplot(aes(x=PCO1, y=PCO2, group=distance, colour=Name)) + 
  geom_point(size=2, pch=19) +
  scale_color_identity() +
  theme(aspect.ratio = 1,
        panel.background = element_blank(),
        legend.position = "none") +
  facet_wrap(~distance, nrow = 1, scales="free",
             labeller = labeller(distance=c(name = "Distance from colour names",
                                            rgb = "Distance from RGB values",
                                            hsv = "Distance from HSV values",
                                            hsl = "Distance from HSL values"))) 

###################################################################################################)
