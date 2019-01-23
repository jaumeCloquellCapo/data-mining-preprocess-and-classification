#Leemos los datos
train <- read.csv("../data/train.csv", header = T, sep = ",", na.strings = "?")
test <- read.csv("../data/test.csv", header = T, sep = ",", na.strings = "?")

#Dimensiones de la train
summary(train) 
dim(train) #Tenemos 9144 instancias con 50 atributos y una clase binaria.
str(train) #Todas las variables son numéricas

#1. DESBALANCEO
table(train$C)

attach(train)

library(ggplot2)
ggplot(train) + geom_bar(aes(x = C, fill = ..count..)) + theme_minimal() + 
  scale_x_continuous(breaks=c(0,1))+
  labs(title = "Diagrama de barras de entidades por etiqueta.", x = "Etiqueta", y = "Conteo", fill ="Cantidad")

## Conclusión -> la proporción es 2/3 1/3 por lo que hay desbalanceo (pero no mucho). 

#2. VALORES PERDIDOS
### Tiene valores perdidos en todas las columnas (menos la clase)

#3. INCONSISTENCIAS
### No sabemos qué son las variables asi que como que no.

#4. RESTO DE ESTUDIO --> GRAFIQUITAS Y COSAS ASI

