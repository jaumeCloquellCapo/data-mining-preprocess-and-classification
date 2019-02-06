# LIMPIEZA DE DATOS

library(randomForest)
library(NoiseFiltersR)
library(kknn)

out_univ <- function(i, train){
  x = train[,i]
  cuantiles <- quantile(x, c(0.25,0.75))
  iqr = IQR(x)
  x[x < cuantiles[1]-5*iqr | x > cuantiles[2]+5*iqr ] <- NA
  return(x)
}

filtrar_univ <- function(train){
  sapply(1:50, out_univ, train)
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

# NO FUNCIONA A?N

outlier_imput <- function(i, test, train){
  x <- train[,i]
  y <- test[,i]
  cuantiles <- quantile(x, c(0.25, 0.75))
  iqr <- IQR(x)
  y[y < cuantiles[1]-5*iqr | y > cuantiles[2]+5*iqr ] <- NA
  formula <- paste("X", i, "~.", sep = "")
  modelo <- kknn(formula, train, test)
  y[is.na(y)] <- modelo$fitted.values[is.na(y)]
  return(y)
}

limpieza_total_test <- function(train, test, iter = 1){
  for(i in 1:iter){
    test <- as.data.frame(sapply(1:50, outlier_imput, test, train))
    colnames(test) <- paste("X",1:50, sep = "")
  }
  return(test)
}