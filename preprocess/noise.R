#paquete NoiseFiltersR
## No trata con Nas. Para ejemplizar: Quitarlos
## Creo que se podría comparar que datos considera ruidosos cada uno.
## También se puedo devolver el conjunto sin ruido.
trainSinNa<-delete_NA(train)
library(NoiseFiltersR)
trainSinNa[,51]<-as.factor(trainSinNa[,51])

## EF (ensemble filter):
ruidoEnsembleFilter<-function(x){
  ensembleFilter<-EF(x)
  return(ensembleFilter$remIdx)
}

## CVCF:

ruidoCVCF<-function(x){
  CVCFFilter<-CVCF(x)
  return(CVCFFilter$remIdx)
}


## IPF:
#(demasiadas, habra que tocar algun parametrio)
ruidoIPF<-function(x){
 IPFFilter<-IPF(x)
  return(IPFFilter$remIdx)
}

## INFFC: (tarda mucho, demasiado)

ruidoINFFC<-function(x){
  INFFCFilter<-INFFC(x)
  return(INFFCFilter$remIdx)
}


