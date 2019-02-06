### FICHERO DE FUNCIONES DE SELECCIÓN DE CARACTERÍSTICAS

## FILTERS

##OJO!!! se considera que la etiqueta está en la primera posicion (por el preprocesamiento de JA)
#cambiar si hace falta

library(FSelector)
library(mlbench)

#Chi.squared

FS_chi.squared <- function(formula,train, test,k=5){
  weights <- FSelector::chi.squared(formula,train)
  subset <- FSelector::cutoff.k(weights,k)

  Y <- train[,1]
  train <- cbind(train[,subset],Y)
  test <- test[,subset]
  return(list(train = train, test = test))
}

#Correlación de Perason
FS_linear_correlation <- function(formula, train, test, k=5){
  x[,51] <- as.numeric(x[,51])-1
  weights <- FSelector::linear.correlation(formula,train)
  subset <- FSelector::cutoff.k(weights,k)
  
  Y <- train[,1]
  train <- cbind(train[,subset],Y)
  test <- test[,subset]
  return(list(train = train, test = test))
}

#Correlación de Spearman
FS_rank_correlation <- function(formula, train, test, k=5){
  weights <- FSelector::rank.correlation(formula,train)
  subset <- FSelector::cutoff.k(weights,k)
  
  Y <- train[,1]
  train <- cbind(train[,subset],Y)
  test <- test[,subset]
  return(list(train = train, test = test))
}

## Basados en entropía
FS_information_gain <- function(formula, train, test, k=5){
  weights <- FSelector::information.gain(formula,train)
  subset <- FSelector::cutoff.k(weights,k)
  
  Y <- train[,1]
  train <- cbind(train[,subset],Y)
  test <- test[,subset]
  return(list(train = train, test = test))
}

FS_gain_ratio <- function(formula, train, test, k=5){
  weights <- FSelector::gain.ratio(formula,train)
  subset <- FSelector::cutoff.k(weights,k)
  
  Y <- train[,1]
  train <- cbind(train[,subset],Y)
  test <- test[,subset]
  return(list(train = train, test = test))
}

FS_symmetrical_uncertainty <- function(formula, train, test, k=5){
  weights <- FSelector::symmetrical.uncertainty(formula,train)
  subset <- FSelector::cutoff.k(weights,k)
  
  Y <- train[,1]
  train <- cbind(train[,subset],Y)
  test <- test[,subset]
  return(list(train = train, test = test))
}

#Relief
FS_relief <- function(formula, train, test, k=5, m=20){
  weights <- FSelector::relief(formula,train, neighbours.count = k, sample.size = m)
  subset <- FSelector::cutoff.k(weights,k)
  
  Y <- train[,1]
  train <- cbind(train[,subset],Y)
  test <- test[,subset]
  return(list(train = train, test = test))
}

## WRAPERS

## AVISO!! El conjunto de entrenamiento se tiene que llamar data_train  y la etiqueta Y

# Se define una funcion de evaluacion: recibe como argumento un 
# vector de atributos a evaluar
# Se define una funcion de evaluacion: recibe como argumento un 
# vector de atributos a evaluar
evaluator <- function(subset){
  # se indica el numero de particiones a realizar en el proceso
  # de validacion cruzada
  k <- 10
  
  # genera valores aleatorios (uniforme) para cada muestra del
  # conjunto de datos
  splits <- runif(nrow(data_train))
  
  # tratamiento de cada una de las particiones. Para cada valor de
  # particion se aplica la funcion que se define a continuacion
  results <- sapply(1:k, function(i) {
    # se determina el indice de las muestras para test (aproximadamente
    # una fraccion 1/k de las muestras del conjunto de datos)
    test.idx <- (splits >= ((i-1)/k) & (splits < (i/k)))
    
    # todas las demas muestras seran para training
    train.idx <- !test.idx
    
    # se seleccionan las muestras en si
    test <- data_train[test.idx, ,drop=FALSE]
    train <- data_train[train.idx, , drop=FALSE]
    
    # aprende el modelo sobre el conjunto de entrenamiento
    tree <- rpart::rpart(as.simple.formula(subset,"Y"),train)
    
    # calcula la tasa de error
    error.rate <- sum(test$Y != predict(tree, test, type="class"))/nrow(test)
    
    # devuelve la tasa de aciertos
    return(1-error.rate)
  })
  
  # se muestra el subconjunto y la media de resultados y se devuelve
  # la media de los resultados (un resultado por particion)
  print(subset)
  print(mean(results))
  return(mean(results))
}

#El conjunto de entrenamiento se tiene que llamar 
FS_best_first <- function(){
  subset <- FSelector::best.first.search(names(data_train)[-51], evaluator) 
}

## AVISO!! El conjunto de entrenamiento se tiene que llamar data_train  y la etiqueta Y

library(rpart)

evaluator_2 <- function(subset, k=5){  
  # genera valores aleatorios (uniforme) para cada muestra del
  # conjunto de datos
  splits <- runif(nrow(data_train))
  
  # tratamiento de cada una de las particiones. Para cada valor de
  # particion se aplica la funcion que se define a continuacion
  results <- sapply(1:k, function(i) {
    # se determina el indice de las muestras para test (aproximadamente
    # una fraccion 1/k de las muestras del conjunto de datos)
    test.idx <- (splits >= ((i-1)/k) & (splits < (i/k)))
    
    # todas las demas muestras seran para training
    train.idx <- !test.idx
    
    # se seleccionan las muestras en si
    test <- data_train[test.idx, ,drop=FALSE]
    train <- data_train[train.idx, , drop=FALSE]
    
    # aprende el modelo sobre el conjunto de entrenamiento
    tree <- rpart(as.simple.formula(subset,"Y"),train)
    
    # calcula la tasa de error
    error.rate <- sum(test$Y != predict(tree,test,type="c"))/nrow(test)
    
    # devuelve la tasa de aciertos
    return(1-error.rate)
  })
  
  # se muestra el subconjunto y la media de resultados y se devuelve
  # la media de los resultados (un resultado por particion)
  print(subset)
  print(mean(results))
  return(mean(results))
}

FS_exhaustive_search <- function(){
  subset <- FSelector::exhaustive.search(names(data_train)[-51], evaluator_2) 
}

FS_forward_search <- function(){
  subset <- FSelector::forward.search(names(data_train)[-51], evaluator_2) 
}

### Hay 3 más


## EMBEDDED

FS_forest_importance <- function(formula, x, k=5, imp = 1){
  weights <- FSelector::random.forest.importance(formula,x, importance.type = imp)
  subset <- FSelector::cutoff.k(weights,k)
  
  Y <- x[,51]
  return(cbind(x[,subset], Y))
}

