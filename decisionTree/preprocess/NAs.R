library(mice)
library(missForest)

#1. MICE
#2. Amelia
#3. missForest
#4. Hmisc
#5. mi

#Función que substitue los N.A pro predicciones
na.mice <- function (dataset) {
  imputed_Data <- mice(dataset, m=5,maxit=50, meth='pmm',seed=500)
  return (complete(imputed_Data,2))
}

na.forest <- function (dataset) {
  mis <- prodNA(dataset, noNA = 0.1)
  imputed_Data <- missForest(mis)
  return (complete(imputed_Data,2))
}

#Función que elimina las filas que continen algún atributo con valor NA
na.delete <- function(dataset){
  return(na.omit(dataset))
}
