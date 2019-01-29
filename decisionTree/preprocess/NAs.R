#Función que elimina las filas que continen algún atributo con valor NA
delete_NA <- function(dataset){
  return(na.omit(dataset))
}
