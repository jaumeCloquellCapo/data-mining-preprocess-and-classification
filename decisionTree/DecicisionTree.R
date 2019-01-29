# Librerias R
library(rpart)
library(rstudioapi)
library(tree)
library(rJava)
library(partykit)
library(dplyr)

# Definimos el path de donde estemos trabajando.
setwd(dirname(getActiveDocumentContext()$path))

# Librerias propias
source("preprocess/lecturaDatos.R")
source("preprocess/discretizacion.R")
source("preprocess/NAs.R")

#Lectura de datos
dataset <- readData(files = c("train.csv", "test.csv"))


####################
##    DEPURACIÓN  ##
####################

# Numero de Missing values
dataset$train %>% select(everything()) %>% summarise_all(funs(sum(is.na(.))))

# Tatamiento de los missign Values
dataset$train <- delete_NA(dataset$train)

# TDetección outliers

########################
##    TRANSFORMACIÓN  ##
########################

# Normalización ?
# si aplicamos K-means como métodos de discretización, es necesario normalizar y escalar los datos

  
#####################
## DISCRETIZACIÓN  ##
##################### 
  
#Discretizamos las variables continuas con modelos no supervisados
dataset$train <- myDiscretization(dataset$train, method = 2)
