library(randomForest)
library(NoiseFiltersR)
library(kknn)
library(tree)
library(rpart)
library(rstudioapi)
library(tree)
library(rJava)
library(partykit)
library(dplyr)
library(party)
library(caret)
library(ipred)
library(RWeka)
library(randomForest)
library(gbm)
# Definimos el path de donde estemos trabajando.
setwd(dirname(getActiveDocumentContext()$path))

# Librerias propias
for (file in list.files("preprocess")) {
  source(paste("preprocess/", file, collapse = NULL, sep = ""))
}

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


FS_forest_importance <- function(formula, x, k=5, imp = 1){
  weights <- FSelector::random.forest.importance(formula,x, importance.type = imp)
  subset <- FSelector::cutoff.k(weights,k)
  
  Y <- x[,51]
  return(cbind(x[,subset], Y))
}

limpieza_total_test <- function(train, test, iter = 1){
  for(i in 1:iter){
    test <- as.data.frame(sapply(1:50, outlier_imput, test, train))
    colnames(test) <- paste("X",1:50, sep = "")
  }
  return(test)
}

limpieza_total_train <- function(train, iter = 1){
  train <- rfImpute(C ~ ., train, iter = 3)
  for(i in 1:iter){
    train[,2:51] <- filtrar_univ(train[,2:51])
    train <- rfImpute(C ~., train, iter = 3)
  }
  train <- filtrar_IPC(train)
  return(train)
}

## función final

original.dataset <- readData()
dataCleaned <- list()

train <- original.dataset$train
test <- original.dataset$test
train$C <- as.factor(train$C)

dataCleaned[["train"]] <- limpieza_total_train(train, 3)
n<-ncol(dataCleaned[["train"]])
input<-dataCleaned[["train"]][ ,-n]
dataCleaned[["trainTomek"]]<- ubBalance(X= input, Y=dataCleaned[["train"]]$C, type="ubTomek")
dataCleaned[["test"]] <- limpieza_total_test(dataCleaned[["train"]], test)

#Create tree model
trees <- tree(C~., dataCleaned[["train"]] )
plot(trees)
text(trees, pretty=0)

#Cross validate to see whether pruning the tree will improve performance
cv.trees <- cv.tree(trees)
plot(cv.trees)

prune.trees <- prune.tree(trees, best=4)
plot(prune.trees)
text(prune.trees, pretty=0)

yhat <- predict(prune.trees, dataCleaned[["test"]] )
plot(yhat, dataCleaned[["test"]]$C)
abline(0,1)
