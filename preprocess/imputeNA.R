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
noNa_train_knn <- knnImputation(train)
write.csv(noNa_train, file = "../data/notNA_train_knn.csv", sep = ",")


## IMPUTACIÓN CON KMEANS
#res <- RKEEL::KMeans_MV(train[,-51], test)
#res$run()

#Buscar SVMI

## PAQUETE MICE (Multivariate Imputation by Chained Equations)

#Imputación con random forest

#Se puede cambiar el método de estimación 
#Solo imputa aquellas variables que no estén altamente correladas (MAR)
miceMod_rf <- mice(train[,-51], method="rf")  # perform mice imputation, based on random forests.
noNa_train_rf <- complete(miceMod_rf)  # generate the completed data.
