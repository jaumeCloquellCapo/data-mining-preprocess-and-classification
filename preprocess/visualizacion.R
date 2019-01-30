###################################
## FUNCIONES PARA VISUALIZACIÓN ##
###################################

library(ggplot2)
library(grid)

multiplot <- function(..., plotlist=NULL, file, cols=3, layout=NULL) {
  
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

densityplot_train <- function(i, datos){
  ggplot(datos) + geom_density(data = datos, mapping = aes(x = datos[,i], fill = C), alpha = 0.5)+ xlab(i)
}
densityplot_test <- function(i, datos){
  ggplot(datos) + geom_density(data = datos, mapping = aes(x = datos[,i]), alpha = 0.5)
}

graficar_variables_train <- function(datos, variables){
  do.call(multiplot, lapply(variables, densityplot_train, datos))
}

graficar_variables_test <- function(datos, variables){
  do.call(multiplot, lapply(variables, densityplot_test, datos))
}


# Si queremos guardar los datos en un pdf podemos ejecutar el siguiente bloque


#pdf("graficos.pdf")
#do.call(multiplot, lapply(2:10, densityplot_train, final))
#do.call(multiplot, lapply(11:19, densityplot_train, final))
#do.call(multiplot, lapply(20:28, densityplot_train, final))
#do.call(multiplot, lapply(29:37, densityplot_train, final))
#do.call(multiplot, lapply(38:46, densityplot_train, final))
#do.call(multiplot, lapply(46:51, densityplot_train, final))
#dev.off()


