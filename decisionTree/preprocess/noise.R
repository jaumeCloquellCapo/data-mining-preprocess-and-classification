library(randomForest)
library(NoiseFiltersR)
library(kknn)


# LIMPIEZA DE DATOS BY JOSE

out_univ <- function(i, train){
  x = train[,i]
  cuantiles <- quantile(x, c(0.25,0.75))
  iqr = IQR(x)
  x[x < cuantiles[1]-3*iqr | x > cuantiles[2]+3*iqr ] <- NA
  return(x)
}

filtrar_univ <- function(train){
  sapply(1:50, out_univ, train)
}

filtrar_IPC <- function(train){
  filtrado <- IPF(C ~ ., train, s = 2)
  return(as.data.frame(filtrado$cleanData))
}

clean_dataset <- function(train, iter = 1){
  train <- rfImpute(C ~ ., train, iter = 3)
  train <- filtrar_IPC(train)
  for(i in 1:iter){
    train[,2:51] <- filtrar_univ(train[,2:51])
    train <- rfImpute(C ~., train, iter = 3)
  }
  return(train)
}


ipfFilter <- function (data) {
  data.outlier <- list()
  trainIPF <- data
  trainIPF[,dim(data)[2]] <- factor(trainIPF[,dim(data)[2]])
  trainIPF <- IPF(C ~., trainIPF, consensus = TRUE, s=2)
  return(trainIPF$cleanData)
}

iqrFilter <- function (dsBase) {
  # ------------------------------
  # Load the data however you want
  # ------------------------------
  
  # I called my data dsBase, here I copy the data to dsBase.iqr
  # I wanted to keep a copy of the original data set
  dsBase.iqr <- dsBase
  
  # Create a variable/vector/collection of the column names you want to remove outliers on.
  vars <- 1:(dim(dsBase)[2] - 1)
  
  # Create a variable to store the row id's to be removed
  Outliers <- c()
  
  # Loop through the list of columns you specified
  for(i in vars){
    
    # Get the Min/Max values
    max <- quantile(dsBase.iqr[,i],0.75, na.rm=TRUE) + (IQR(dsBase.iqr[,i], na.rm=TRUE) * 1.5 )
    min <- quantile(dsBase.iqr[,i],0.25, na.rm=TRUE) - (IQR(dsBase.iqr[,i], na.rm=TRUE) * 1.5 )
    
    # Get the id's using which
    idx <- which(dsBase.iqr[,i] < min | dsBase.iqr[,i] > max)
    
    # Output the number of outliers in each variable
    # print(paste(i, length(idx), sep=''))
    
    # Append the outliers list
    Outliers <- c(Outliers, idx) 
  }
  
  # Sort, I think it's always good to do this
  #Outliers <- sort(Outliers)
  
  # Remove the outliers
  dsBase.iqr <- dsBase.iqr[-Outliers,]
  return (dsBase.iqr)
}