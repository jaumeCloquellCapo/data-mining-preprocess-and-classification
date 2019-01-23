### FICHERO DE FUNCIONES DE NORMALIZACIÓN

min_max <- function(x){
  x <- (x - min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm =T))
}

min_max_bd <- function(x){
  x <- data.frame(sapply(x, min_max))
}

z_score_bd <- function(x){
  x <- data.frame(sapply(x, scale))
}
