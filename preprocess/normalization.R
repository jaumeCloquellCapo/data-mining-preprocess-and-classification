### FICHERO DE FUNCIONES DE NORMALIZACIÓN

#Función que aplica normalización min-max a toda la base de datos
min_max_db <- function(train, test){
  for (i in 1:50){
    min <- min(train[,i], na.rm = T)
    max <- max(train[,i], na.rm = T)
    
    train[,i] <- (train[,i]-min)/(max-min)
    test[,i] <- (test[,i]-min)/(max-min)
  }
  
  return(list(train,test))
}


z_score_bd <- function(x){
  x <- data.frame(sapply(x, scale))
}
