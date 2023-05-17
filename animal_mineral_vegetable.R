library(tidyverse)
library(randomForest)


train <- data.frame(class=factor(c(rep("vegetable", times=10), rep("mineral", times=10), rep("animal", times=10))),
                    predictor=c(rep("broccoli", times=10), rep("copper", times=10), rep("albatross", times=10))) 

rfmod <- randomForest(class ~ predictor, data=train)

test <- data.frame(predictor=c("albatross", "broccoli", "copper"))
bind_cols(test, prediction=predict(rfmod, test))

test2 <- data.frame(predictor=c("broccoli", "copper"))
bind_cols(test2, prediction=predict(rfmod, test2))

test3 <- data.frame(predictor=c("albatross", "broccoli", "copper", "dog"))
bind_cols(test3,prediction=predict(rfmod, test3))

test4 <- data.frame(predictor=c("albatross", "broccoli", "copper", "ant"))
bind_cols(test4,prediction=predict(rfmod, test4))




