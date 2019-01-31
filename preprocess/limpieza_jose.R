# LIMPIEZA DE DATOS

library(randomForest)
library(NoiseFiltersR)

out_univ <- function(i, train){
  x = train[,i]
  cuantiles <- quantile(x, c(0.25,0.75))
  iqr = IQR(x)
  x[x < cuantiles[1]-3*iqr | x > cuantiles[2]+3*iqr ] <- NA
  return(x)
}

filtrar_univ <- function(train){
  sapply(1:50, train)
}

filtrar_IPC <- function(train){
  filtrado <- IPF(C ~ ., train, s = 2)
  return(as.data.frame(filtrado$cleanData))
}

limpieza_total_train <- function(train, iter = 1){
  train <- rfImpute(C ~ ., train, iter = 3)
  train <- filtrar_IPC(train)
  for(i in 1:iter){
    train[,2:51] <- filtrar_univ(train[,2:51])
    train <- rfImpute(C ~., train, iter = 3)
  }
  return(train)
}
