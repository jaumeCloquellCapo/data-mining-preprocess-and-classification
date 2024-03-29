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

  train <- rfImpute(C~., train, iter = 1)
  C<-as.factor(train$C)
  for(i in 1:iter){
    
    train<-train[,-1]
    train[,1:50] <- filtrar_univ(train[,1:50])
    train<-cbind(train, C)
    train <- rfImpute(C~., train, iter = 1)
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

dataCleaned[["train"]] <- limpieza_total_train(train, 1)
#n<-ncol(dataCleaned[["train"]])
#input<-dataCleaned[["train"]][ ,-n]
##dataCleaned[["trainTomek"]]<- ubBalance(X= input, Y=dataCleaned[["train"]]$C, type="ubTomek")
dataCleaned[["test"]] <- limpieza_total_test(dataCleaned[["train"]][,-1], test)

hr_base_model <- rpart(C ~ ., data = dataCleaned[["train"]], method = "class",
                       control = rpart.control(cp = 0))
pred <- predict(hr_base_model, dataCleaned[["test"]], type = "class")

summary(hr_base_model)

#Plot Decision Tree
rpart.plot(hr_base_model)

# Examine the complexity plot
printcp(hr_base_model)
plotcp(hr_base_model)

bestcp <- hr_base_model$cptable[which.min(hr_base_model$cptable[,"xerror"]),"CP"]
bestcp
hr_model_pruned<- prune(hr_base_model, cp= bestcp)
rpart.plot(hr_model_pruned)
pred <- predict(hr_model_pruned, dataCleaned[["test"]], type = "class")
KaggleWiteData(1:dim(dataCleaned[["test"]])[1], pred, path = "predictions/")



