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
for (file in list.files("preprocess")) {
  source(paste("preprocess/", file, collapse = NULL, sep = ""))
}

#Lectura de datos
dataset <- readData(files = c("train.csv", "test.csv"))
dataset$train$C <- as.factor(dataset$train$C)

####################
##    DEPURACIÓN  ##
####################

# Numero de Missing values
dataset$train %>% select(everything()) %>% summarise_all(funs(sum(is.na(.))))

# Tatamiento de los missign Values
dataset$train <- na.mice(dataset$train)

# Selección de variables, reducción de dimensionalidad

# TDetección outliers
dataset$train <- delete_ruidoCVCF(dataset$train)


########################
##    TRANSFORMACIÓN  ##
########################

# Normalización ?
# si aplicamos K-means como métodos de discretización, es necesario normalizar y escalar los datos

#Se aplica el centrado y escalado sobre el conjunto de datos, una vez eliminados los valores perdidos
valoresPreprocesados <- caret::preProcess(dataset$test[, -c(dim(dataset$train)[2] - 1)],method=c("center","scale"))
dataset$train[, -c(dim(dataset$train)[2] - 1)] <- predict(valoresPreprocesados,dataset$train[, -c(dim(dataset$train)[2] - 1)])

  
#####################
## DISCRETIZACIÓN  ##
##################### 
  
#Discretizamos las variables continuas con modelos no supervisados
dataset$train[,1] <- myDiscretization(dataset$train, method = 2)

#####################
##     MODELO     ##
##################### 

# construye el modelo
library(party)
library(caret)
ct <- ctree(C~., dataset$train)
print(ct)
plot(ct)

# se realiza la prediccion
model <- predict(ct, dataset$test)
witeData(1:dim(dataset$test)[1], model, path = "predictions/")
