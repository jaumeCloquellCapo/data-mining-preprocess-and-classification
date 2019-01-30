### FICHERO DE FUNCIONES DE IMPUTACIÓN DE NAs

#Las funciones programadas por nosotros no imputan valores perdidos en test pues no hay.

#La función de eliminar filas con más de un 5% de los valores perdidos no hace falta pues no hay
#ninguna fila.

#Librerías necesarias
library(mice)
require(robCompositions)
require(Amelia)
require(randomForest)


#Función que elimina las filas que continen algún atributo con valor NA
#No se eliminan más de una clase que de otra en proporción
#El desbalanceo sigue siendo el mismo.
delete_NA <- function(x){
  x <- x[complete.cases(x),]
}

### IMPUTAR CON VALORES ESTADÍSTICOS

#Función que imputa con algún método de mice
#Problema con datos muy correlados
impute_mice <- function(x, metodo){
  return(mice::mice(x, m=5, meth = metodo))
}

#Función que imputa con la media los valores perdidos de una columna (atributo)
replace_mean_NA <- function(x){
  replace(x, is.na(x), mean(x, na.rm = TRUE))
}


#Función que imputa con la media de cada atributo todos los valores perdidos en la bd.
replace_mean_bd_NA <- function(x){
  x <- sapply(x,replace_mean_NA)
}


#Función que imputa con la mediana los valores perdidos de una columna (atributo)
replace_median_NA <- function(x){
  replace(x, is.na(x), median(x, na.rm = TRUE))
}

#Función que imputa con la mediana de cada atributo todos los valores perdidos en la bd.
replace_median_bd_NA <- function(x){
  x <- sapply(x,replace_median_NA)
}

#Función que imputa con la mediana los valores perdidos de una columna (atributo)
replace_mode_NA <- function(x){
  replace(x, is.na(x), mode(x, na.rm = TRUE))
}

#Función que imputa con la mediana de cada atributo todos los valores perdidos en la bd.
replace_mode_bd_NA <- function(x){
  x <- sapply(x,replace_mode_NA)
}

#Imputación con randomForest
impute_rf <- function(formula, x, iter = 5){
  return(rfImpute(formula, x, iter))
}

#Imputación con robComposition
#Se aplica solo sobre variables numéricas
#(quitar etiqueta antes de llamar)
impute_KNNa <- function(x){
  imputados <- robCompositions::impKNNa(x, primitive=TRUE)
  return(imputados$xImp)
}

#Imputaci�n con Amelia <- es r�pido

impute_amelia <- function(x, iter = 5){
  imputados <- amelia(x)
  return(as.data.frame(imputados$imputations[[5]]))
}