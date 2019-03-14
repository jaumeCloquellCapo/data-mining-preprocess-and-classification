library(mice)
library(missForest)



#Función que elimina las filas que continen algún atributo con valor NA
na.delete <- function(dataset){
  return(na.omit(dataset))
}


imputacionKnnTotal <- function(x){
  #' Se incluye dentro una primera función imputacionKnn
  
  imputacionKnn <- function(x, y){
    #' Se le pasa una matriz o data.frame x y el índice de una de sus columnas
    #' Busca en la columna las instancias perdidas y para esas las variables que tienen valores perdidos de cara 
    #' a que el test no de fallos ya que como no hay más de un valor perdido por instancia será como
    #' máximo el mismo número de instancias perdidas.
    #' Se buscan las instancias con valores perdidos para dichas variables y se omiten para el train,
    #' estando entre ellas evidentemente las que vamos a predecir.
    #' Se construyen train y test y se entrena con K-NN, usando CV y 10 repeticiones, el resultado
    #' es la matriz o data.frame original pero con dicha columna modificada.
    
    require(caret)
    
    # Instancia perdida de la columna
    instanciasPerdidas <- which(is.na(x[,y])|x[,y]=="")
    # Otras variables con datos perdidos en dichas instancias
    variablesPerdidas <- which(sapply((1:dim(x)[2])[-y], function(z) any(is.na(x[instanciasPerdidas,z]) | x[instanciasPerdidas,z]=="")))
    
    # Búsqueda de instancias con perdidos obviando, en caso de que estén, aquellas variables descartadas
    if(length(variablesPerdidas)!=0){
      instanciasX <- sapply(1:dim(x)[1], function(z) sum(is.na(x[z,-variablesPerdidas]))+sum(x[z,-variablesPerdidas]=="", na.rm = TRUE))
    } else {
      instanciasX <- sapply(1:dim(x)[1], function(z) sum(is.na(x[z,]))+sum(x[z,]=="", na.rm = TRUE))
    }
    
    # Quedarme con los índices de las instancias con perdidos
    instanciasX <- which(instanciasX!=0)
    
    if(length(variablesPerdidas)!=0){
      train <- x[-instanciasX, -c(y,variablesPerdidas)]
      test <- x[instanciasPerdidas, -c(y,variablesPerdidas)]
    } else {
      train <- x[-instanciasX,-y]
      test <- x[instanciasPerdidas,-y]
    }
    
    train.class <- x[-instanciasX,y]
    
    
    variablesNumericas <- which(sapply(1:dim(train)[2], function(z) is.numeric(train[,z])))
    # Elimino la clase en caso de que esté entre ellas
    variablesNumericas <- variablesNumericas[!variablesNumericas==y]
    
    modelo <- caret::train(train[,variablesNumericas], train.class,
                           method = "knn",
                           tuneLength = 10,
                           trControl = trainControl(method = "cv"))
    
    modelo.predict <- predict(modelo,test[,variablesNumericas])
    if(is.factor(modelo.predict)){
      x[instanciasPerdidas,y] <- as.character(modelo.predict)
    } else {
      x[instanciasPerdidas,y] <- modelo.predict
    }
    
    x
  }
  #' Segunda parte de la función:
  #' Le paso todas las varaibles perdidas a la función anterior
  #' para ello voy pasando el mismo data.frame, con los datos originales en cada iteración
  #' y en cada una de ellas voy sustituyendo en una copia del dataframe los perdidos con
  #' las imputaciones.
  
  y <- x
  variablesPerdidas <- which(sapply((1:dim(x)[2]), function(z) any(is.na(x[,z]) | x[,z]=="")))
  for(i in variablesPerdidas){
    print(i)
    n <- imputacionKnn(x,i)
    y[,i] <- n[,i]
  }
  y
}