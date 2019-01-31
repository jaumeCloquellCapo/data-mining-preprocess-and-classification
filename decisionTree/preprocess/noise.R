## También se puedo devolver el conjunto sin ruido.
library(NoiseFiltersR)

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