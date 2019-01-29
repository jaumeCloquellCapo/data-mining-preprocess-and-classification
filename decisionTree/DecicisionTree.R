# Librerias R
library(rpart)
library(rstudioapi)
library(tree)
library(rJava)
library(partykit)

# Definimos el path de donde estemos trabajando.
setwd(dirname(getActiveDocumentContext()$path))

# Librerias propias
source("preprocess/lecturaDatos.R")
source("preprocess/discretizacion.R")

#Lectura del dataset de train y testing
dataset <- readData(files = c("train.csv", "test.csv"))

#Tratamiento NA


#Discretizamos las variables continuas
str(dataset$train)
pruebas <- myDiscretization(dataset$train, print = TRUE)
str(pruebas)
pruebas <- myDiscretization(dataset$train, method = 2, print = TRUE)
str(pruebas)
pruebas <- myDiscretization(dataset$train, method = 2, print = TRUE)
str(pruebas)