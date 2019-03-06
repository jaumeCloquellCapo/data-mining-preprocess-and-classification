#Cargamos bibliotecas

library(caret)
library(randomForest)
library(NoiseFiltersR)
library(Amelia)
library(dataPreparation)
library(class)
library(kknn)
library(imbalance)

#Fijamos el directorio donde trabajamos

#setwd("C:/Users/Jose/Documents/GitHub/DataMining/preprocess")

#Ejecutamos otros script con las funciones que utilizamos

source('imputeNA.R')
source('limpieza_jose.R')
source('normalization.R')

#se leen los datos, se convierte en factor la etiqueta y se preprocesan los datos con imputación, IPF y
#corrección de outliers

datos <- read.csv("train.csv", na.strings = c("?","NA",".",""))
datos[,51]=as.factor(datos[,51])
datos <- limpieza_total_train(datos, iter = 3)

#se leen los datos de test y se aplica una corrección de outliers sobre ellos

test <- read.csv("test.csv", na.strings = c("?","NA",",","."))
test <- limpieza_total_test(datos[,2:51], test, iter = 3)

#se normalizan los datos

normalizacion <- z_score_bd(datos, test)
test <- normalizacion[[2]]
train <- normalizacion[[1]]

#se entrena el método con cross validation

trcontrol <- trainControl(method = 'repeatedcv', number = 10, repeats = 3 )
fit_knn <- train(C ~., 
                 data = train, method = "knn",
                 trControl = trcontrol, preProcess = c("center","scale"), 
                 tuneGrid = expand.grid(k = c(7,9,11,13,15)))

#se predicen los resultados y se escriben en un archivo para subirlo a la plataforma

resultados <- predict(fit_knn, test)
subida<- data.frame(Id = 1:3919, Prediction = as.numeric(resultados)-1)
write.csv(subida, "ejemplo.csv", row.names = FALSE, quote = FALSE)
