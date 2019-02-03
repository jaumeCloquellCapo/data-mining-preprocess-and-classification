# Librerias R
library(rpart)
library(rstudioapi)
library(tree)
library(rJava)
library(partykit)
library(dplyr)
library(party)
library(caret)

# Definimos el path de donde estemos trabajando.
setwd(dirname(getActiveDocumentContext()$path))

# Librerias propias
for (file in list.files("preprocess")) {
  source(paste("preprocess/", file, collapse = NULL, sep = ""))
}

#Lectura de datos
dataset <- readData(files = c("train.csv", "test.csv"))
dataset$train$C <- as.factor(dataset$train$C)

#Dimensiones de la train
summary(dataset$train) 
## Todas las variables tienen Missing Values y además nos ncontramos cpn outliers ne la mayoría de ellos. x1,x2,x3...

dim(dataset$train) #Tenemos 9144 instancias con 50 atributos y una clase binaria.
str(dataset$train) #Todas las variables son numéricas

#1. DESBALANCEO
table(dataset$train$C)

####################
##    DEPURACIÓN  ##
####################

## N.A

## Estudio de los N.A

# Numero de Missing values
apply(dataset$train,2,function(x){
  sum(is.na(x))/length(x)*100
}) %>% sort

## Todas la variables tienen N.A [20%, 54%]
# Variables con más N.A
#X9 -> 54%
#X32 -> 44%
#X40 -> 41%

# Tatamiento de los missign Values

## If a variable has less than 50% missing values, imputation or prediction might be a viable option.
## If a variable has more than 50% missing values, drop the variable because you don’t have enough information to ascertain anything useful. 
dataset$inputed <- subset(dataset$train, select = -c(X9))

set.seed(81)
iris.mis <- prodNA(dataset$inputed, noNA = 0.2)
summary(iris.mis)
iris.imp <- missForest(iris.mis, xtrue = iris, verbose = TRUE)

# Selección de variables, reducción de dimensionalidad

# TDetección outliers


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
#dataset$train[,1] <- myDiscretization(dataset$train, method = 2)

#####################
##     MODELO     ##
##################### 

# construye el modelo

ct <- ctree(C~., dataset$train)
cpus.ltr.cv <- cv.tree(ct, fun = prune.tree, K = 10)
print(ct)
plot(ct)

# se realiza la prediccion
test_predict <- predict(ct, dataset$test)

# Classification Tree with rpart
library(rpart)

# grow tree 
fit <- rpart(C~., method="class", data=dataset$train)
summary(fit, cp = 0.06)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits
rpart.plot(fit)
rpart.rules(fit)

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree 
pfit<- prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Kyphosis")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


# Random Forest prediction of Kyphosis data
library(randomForest)
fit <- randomForest(C~.,data=dataset$train, na.action=na.omit)
print(fit) # view results 
importance(fit) # importance of each predictor


# Conditional Inference Tree for Kyphosis
library(party)
fit <- ctree(C~.,data=trainData)
plot(fit, main="Conditional Inference Tree for Kyphosis",varlen=3)
