source("estudio_bd.R")
source("imputeNA.R")
source("normalización.R")
source("EDA.R")

train <- read.csv("../data/train.csv", header = T, sep = ",", na.strings = "?")
test <- read.csv("../data/test.csv", header = T, sep = ",", na.strings = "?")

apply(test, 2, iDiscretization)