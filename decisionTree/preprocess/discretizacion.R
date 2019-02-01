require(discretization)
require(arules)

# Función para discretizar las variables
# methods: 1 CAIM, 2 CACC, 3 AMEVA
myDiscretization <- function (dataset, method = 1) {
  for (col in colnames(dataset)) {
    p  apply(row, 2, function(x) {
      print(x)
      #mastercuts <- arules::discretize(x, method = "interval", categories = 4, onlycuts = T)
      #dataset[[x]] <- as.numeric(cut(dataset[[x]], breaks = mastercuts))
    })
  }
  
  #dd <- disc.Topdown(dataset, method=method)  # Tarda demasiado
  #print(cm$cutp)
  # print(dd$cutp)
  return(dataset)
}

