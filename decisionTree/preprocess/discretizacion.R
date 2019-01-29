require(discretization)

# Función para discretizar las variables
# methods: 1 CAIM, 2 CACC, 3 AMEVA
myDiscretization <- function (dataset, method = 1, print = FALSE) {
  dd <- discretization::disc.Topdown(dataset, method=method)
  print(dd$cutp)
  return(dd)
}

