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



library(C50)
credit.fit <- C5.0(dataCleaned[["train"]], dataCleaned[["train"]]$C)
credit.fit

#Create tree model
trees <- tree(C~., dataCleaned[["train"]] )
plot(trees)
text(trees, pretty=0)
summary(trees)$used
names(dataCleaned[["train"]])[which(!(names(dataCleaned[["train"]]) %in% summary(trees)$used))]

trees <- tree(C ~ . -X2 -X32 -X17, dataCleaned[["train"]] )
#Cross validate to see whether pruning the tree will improve performance
cv.trees <- cv.tree(trees)
plot(cv.trees)

prune.trees <- prune.tree(trees, best=10)
plot(prune.trees)
text(prune.trees, pretty=0)

yhat <- predict(prune.trees, dataCleaned[["test"]], type = "class")
plot(yhat, dataCleaned[["test"]]$C)
abline(0,1)

KaggleWiteData(1:dim(test)[1], yhat, "predictions/")

## priobamos con selector de variables


library(RWeka)
library(caret)

originalCleantrain <- read.csv("datosFiltrados/cleantrain.csv", header = T, sep = ",", na.strings = c("?","NA",".",""))
originalCleantest <- read.csv("datosFiltrados/cleantest.csv", header = T, sep = ",", na.strings = c("?","NA",".",""))
cleantrain <- originalCleantrain[,2:dim(originalCleantrain)[2]]
cleantest <- originalCleantest[,2:dim(originalCleantest)[2]]
cleantrain$C <- as.factor(cleantrain$C)

library(unbalanced)
n<-ncol(cleantrain)
output<-cleantrain$C
input<-cleantrain[ ,-n]

data<-ubTomek(X = input, Y = output)
data<-cbind(data$X, "C" = data$Y)

data[,1:50] <- filtrar_univ(data[,1:50])
data <- rfImpute(C ~., data, iter = 3)
cleantrain <- data

hr_base_model <- rpart(C ~ ., data = cleantrain, method = "class",
                       control = rpart.control(cp = 0))
printcp(hr_base_model)
bestcp <- hr_base_model$cptable[which.min(hr_base_model$cptable[,"xerror"]),"CP"]
bestcp
hr_model_pruned<- prune(hr_base_model, cp= bestcp)
test$pred <- predict(hr_model_pruned, test, type = "class")
accuracy_postprun <- mean(test$pred == test$left)
KaggleWiteData(1:dim(cleantest)[1], test$pred, path = "predictions/")
