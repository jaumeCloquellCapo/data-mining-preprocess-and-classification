
#LECTURA DE DATOS
#setwd("~/Desktop/Pre_Cla/DataMining/RIPPER")
train <- read.csv("../data/train.csv", header = T, sep = ",", na.strings = "?")

test <- read.csv("../data/test.csv", header = T, sep = ",", na.strings = "?")

#Pasamos a factor la clase
train[,51] <- as.factor(train[,51])

#Filtrado de instancias con cierto porcentaje de datos perdidos

#Función que devuelve porcentaje de datos perdidos por fila
library(parallel)
system.time(res1 <- apply(train, 1, function(x) sum(is.na(x))) / ncol ( train ) * 100)

#No hay filas con m´ás de un 5% de los datos perdidos
mal <- (res1 > 5)
filtrados <- train[!mal,]

library(mice)

#train_completo <- mice::mice(train, m=5, meth = "pmm")

attach(train)
#Imputación con knna
#train_completo_knna <- impute_KNNa(train[,-51])
#write.csv(train_completo_knna, file = "completo_knna.csv", row.names = F)
train_completo_knna <- read.csv("completo_knna.csv", sep = ",")

X <- data.frame(train_completo_knna)
Y <- train[,51]

normalized_data <- z_score_bd(X, test)
#Escalamos y centramos train y test con respecto a medidas de train.
X <- data.frame(normalized_data[1])
test <- data.frame(normalized_data[2]) #### Las medias están bastante alejadas de 0

data_train <- cbind(X,Y) 


#data_train <- delete_ruidoCVCF(data_train)
X <- data_train[,1:50]
Y <- data_train[,51]




data_train <- cbind(X,Y)


data_features<- FS_forest_importance(Y~., data_train, k=30)

#res<-conjuntoDiscretizado(data_features, 3)
library(RWeka)
library(caret)
TrainCtrl1 <- trainControl(method = "repeatedcv", number = 5, repeats=5)

Ripper <- train(Y ~., data = data_features, method = "JRip", trControl=TrainCtrl1, verbose = T)
