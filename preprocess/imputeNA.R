### FICHERO DE FUNCIONES DE IMPUTACIÓN DE NAs

#Las funciones programadas por nosotros no imputan valores perdidos en test pues no hay.

#Función que elimina las filas que continen algún atributo con valor NA
#No se eliminan más de una clase que de otra en proporción
#El desbalanceo sigue siendo el mismo.
delete_NA <- function(x){
  x <- x[complete.cases(x),]
}

### IMPUTAR CON VALORES ESTADÍSTICOS

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

## IMPUTACIÓN CON KNN
#train: train sin la etiqueta
#test: test
knn_MV_NA <- function(train, test, k=10){
  res <- RKEEL::KNN_MV(train, test, k)
  res$run()
  return(res)
} 

##Generación del archivo con imputación knn

#Normalizamos los datos
normalized_data <- min_max_db(train,test)
normalized_train <- data.frame(normalized_data[1])
normalized_test <- data.frame(normalized_data[2])

#Quitamos etiqueta
normalized_train_data <- normalized_train[,1:50]

#Imputamos valores
preprocessed_data <- knn_MV_NA(normalized_train_data, normalized_test)
