
# Funcion para lectura de archivo.
readData <- function(path = "../data/", files = c("train.csv", "test.csv")){
  # inicializamos la variable de respuesta
  datasets <- list()
  datasetValues <- c()
  datasetNames <- c()
  
  for(file in files){
    # se compone el path completo
    pathCompleto <- paste(path,file,sep="")
    # se leen los datos
    datasetValue <- read.csv(pathCompleto, header = T, sep = ",", na.strings = "?")
    datasetName <- unlist(strsplit(file, split='.', fixed=TRUE))[1]
    datasets[[datasetName]] <- datasetValue
  }
  
  return(datasets)
}

