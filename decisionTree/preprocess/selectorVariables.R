library(mlbench)
library(FSelector)

selector.random.forest <- function (formula, data, importance.type = 1) {
  weights <- FSelector::random.forest.importance(C~., data, importance.type=importance.type)
  # se muestran los resultados
  print(weights)
  subset <- cutoff.k(weights,dim(data)[2])
  f <- as.simple.formula(subset,"Class")
  print(f)
}


selector.chiSquare <- function (data) {
  weights <- FSelector::chi.squared(C~.,data)
  print(weights)
  
  # se seleccionan los 5 mejores
  subset <- FSelector::cutoff.k(weights,dim(data)[2])
  
  # se muestran los seleccionados
  f <- as.simple.formula(subset,"Class")
  print(f)
  
  
}