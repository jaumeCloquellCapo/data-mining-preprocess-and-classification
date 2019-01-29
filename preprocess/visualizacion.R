# Visualización de las funciones de densidad de las variables

# Cargamos las librerías que vamos a utilizar

library(ggplot2)
library(NoiseFiltersR)
library(Amelia)

# Leemos los datos

datos <- read.csv("train.csv", na.strings = c("?","NA",".",""))
datos[,51]=as.factor(datos[,51])

# Imputamos con Amelia

imputados_am <- amelia(datos, idvars = "C")
imputados_am <- as.data.frame(imputados_am$imputations[[5]])

# Filtramos con IPF

filtrados_IPF <- IPF(C ~ ., imputados_am, s = 2)
filtrados_IPF <- as.data.frame(filtrados_IPF$cleanData)

# Definimos las funciones con las que vamos a graficar las funciones de densidad

multiplot <- function(..., plotlist=NULL, file, cols=3, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# En los conjuntos de training podemos distinguir entre las distintas clases de C
# pero en test no

densityplot_train <- function(i, datos){
  ggplot(datos) + geom_density(data = datos, mapping = aes(x = datos[,i], fill = C), alpha = 0.5)
}
densityplot_test <- function(i, datos){
  ggplot(datos) + geom_density(data = datos, mapping = aes(x = datos[,i]), alpha = 0.5)
}

# Los plots de densidad son muy malos aún porque sigue habiendo muchos outliers en los datos
# <- hay que limpiar mejor

do.call(multiplot, lapply(1:3, densityplot_train, filtrados_IPF))


# Comparamos con las distribuciones de test
# También hay mucho ruido, tal vez hay que procesar algo también en test

test <- read.csv("test.csv", na.strings = c("?","NA",".",""))
do.call(multiplot, lapply(3:5, densityplot_test, test))

