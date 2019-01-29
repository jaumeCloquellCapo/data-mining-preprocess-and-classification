#paquete NoiseFiltersR
## No trata con Nas. Para ejemplizar: Quitarlos
## Creo que se podría comparar que datos considera ruidosos cada uno.
## También se puedo devolver el conjunto sin ruido.
library(NoiseFiltersR)

## EF (ensemble filter):
#Llama a varios clasificadores y cuando la mayoría de ellos lo clasifica mal --> se considera ruido
ruidoEnsembleFilter<-function(x){
  ensembleFilter<-EF(x)
  return(ensembleFilter$remIdx)
}

## CVCF:
#Igual pero con CV
ruidoCVCF<-function(x){
  CVCFFilter<-CVCF(x)
  return(CVCFFilter$remIdx)
}

delete_ruidoCVCF<-function(x){
  CVCFFilter<-CVCF(x)
  return(CVCFFilter$cleanData)
}


## IPF:
#Múltiples iteraciones de CVCF
ruidoIPF<-function(x){
 IPFFilter<-IPF(x)
  return(IPFFilter$remIdx)
}

## INFFC: 
## Devuelve directamente el conjunto limpio de outliers
## HAY QUE TENER MUCHA PACIENCIA
ruidoINFFC<-function(x){
  INFFCFilter<-INFFC(x)
  return(INFFCFilter$cleanData)
}

#Función que elimina aquellos datos que han sido considerados outliers
#por todos los métodos
delete_ruido_todas <- function(x){
  idx_1 <- ruidoEnsembleFilter(x)
  idx_2 <- ruidoCVCF(x)
  idx_3 <- ruidoIPF(x)
  #idx_4 <- ruidoINFFC(x)
  
  common_idx <- intersect(intersect(idx_1,idx_2),idx_3)
  
  x <- x[-common_idx,]  
}


