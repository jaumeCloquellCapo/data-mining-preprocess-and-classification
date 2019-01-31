library(mlbench)
library(FSelector)

selector.random.forest <- function (formula, data, importance.type = 1) {
  weights <- FSelector::random.forest.importance(formula, data, importance.type=importance.type)
  # se muestran los resultados
  print(weights)
  subset <- cutoff.k(weights,5)
  f <- as.simple.formula(subset,"Class")
  print(f)
}