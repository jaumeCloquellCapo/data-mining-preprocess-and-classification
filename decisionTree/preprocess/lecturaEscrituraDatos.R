source("preprocess/constants.R")
# Funcion para lectura de archivo.
readData <- function(path = "../data/", files = c("train.csv", "test.csv"), fileNames = c(const$train, const$test)){
  # inicializamos la variable de respuesta
  datasets <- list()
  #datasetValues <- c()
  #datasetNames <- c()
  
  index <- 1
  for(file in files){
    # se compone el path completo
    pathCompleto <- paste(path,file,sep="")
    # se leen los datos
    datasetValue <- read.csv(pathCompleto, header = T, sep = ",", na.strings = c("?","NA",".",""))
    if  (length(fileNames) == length(files)) {
      datasetName <- fileNames[index]
    } else {
      datasetName <- unlist(strsplit(file, split='.', fixed=TRUE))[1]
    }
    index <- index + 1 
    datasets[[datasetName]] <- datasetValue
  }
  
  return(datasets)
}

KaggleWiteData <- function (ids, predictModel, path = "") {
  # create a dataframe with our results
  my_submission <- data.frame(Id = ids, Prediction = as.numeric(predictModel) - 1) 
  write.csv(my_submission, file = paste(path, format(Sys.time(), "%X %d-%m-%Y"), ".csv", sep = " "), sep = " ", row.names = FALSE, col.names=TRUE, quote = FALSE)
}


writeData <- function(dataset, file, path = ""){
  write.csv(dataset, file = paste(path,file,".csv", sep=""), row.names = FALSE)
}
