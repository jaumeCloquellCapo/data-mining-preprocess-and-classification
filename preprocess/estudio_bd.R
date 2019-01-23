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

#2. INCONSISTENCIAS
### No sabemos qué son las variables asi que como que no.


#3. NORMALIZACIÓN
## Distintas normalizaciones en normalization.R

#4. VALORES PERDIDOS
### Tiene valores perdidos en todas las columnas (menos la clase)
### Métodos para solventarlo en imputeNA.R

#5: RUIDO (por ejemplo el SVM no es robusto a ruido) 

#6.REDUCCIÓN DE LA DIMENSIONALIDAD

#OPC 1: Hay algunas variables muy muy correladas (eliminar todas a chuchillo) 
#y otras que son unas cosas normales
noNa_train <- read.csv(file = "../data/notNA_train_knn.csv", sep = ",", header = T)
corrplot.mixed(cor(noNa_train_knn))

#OPC 2: Análisis de componentes principales. Aplicar PCA pa ver qué nos dice.
#Sabemos que hay variables que eliminaríamos pero seguramente el PCA nos de una mejor combinación.

#OPC 3: MDS

#OPC 4: Kernel PCA

#OPC 5: LLE

#OPC 6: Autoencoder

#OPC 7: más alg del estilo.

#Probar nuestro modelo vs. modelo obtenido con PCA y el resto de algoritmos.

#7. SELECCIÓN DE INSTANCIAS:

#Mirar si hay instancias repetidas (en tal caso eliminamos).

#Mirar si hacemos oversampling / undersampling

#Discretización -> Para reglas y árboles.

