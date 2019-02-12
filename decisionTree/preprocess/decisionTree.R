library(tree)
library(rpart)
library(rJava)
library(partykit)
library(dplyr)
library(party)
library(caret)
library(ipred)
library(MASS)
library(TH.data)
library(C50)
library(RWeka)
source("preprocess/constants.R")

#'@function crear_sets
#'@description Generao las particiones de train y test
#'
#'@param datos Dateset orginal
#'@param proporcion Procentaje de datos en test y trrain
crear_sets <- function(datos, proporcion = .7) {
  results <- list()
  
  results[[const$train]] <- sample_frac(datos, proporcion)
  results[[const$test]] <- setdiff(datos, results[[const$train]])
  
  return (results)
}

#'@function generar_formula
#'@description Genera la formula para los arboles de clasificacion
#'
#'@param objetivo Variable a predecir
#'@param predictores Predictores
generar_formula <- function(objetivo, predictores = ".") {
  if(length(predictores > 1)) {
    predictores <- paste0(predictores, collapse = "+")
  }
  formula <- paste0(objetivo, " ~ ", predictores) %>% as.formula()
  return (formula)
}

#'@function entrenar_arbol_rpart
#'@description Genera la formula para los arboles de clasificacion
#'
#'@param objetivo Variable a predecir
#'@param predictores Predictores
#'
entrenar_arbol_rpart <- function(sets, objetivo, predictores = ".") {
  results <- list()
  
  results[[const$model]] <- rpart(formula = generar_formula(objetivo, predictores = predictores),data = sets[[const$train]])
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  
  return (results)
}

#'@function entrenar_arbol_c45
#'@description The C4.5 algorithm is an extension of the ID3 algorithm and constructs a decision tree to maximize information gain (difference in entropy).
#'
#'@param objetivo Variable a predecir
#'@param predictores Predictores
#'
entrenar_arbol_c45 <- function(sets, objetivo, predictores = ".") {
  results <- list()
  
  results[[const$model]] <- J48(formula = generar_formula(objetivo, predictores = predictores), data = sets[[const$train]])
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  
  return (results)
}

#'@function entrenar_arbol_PART
#'@description The C4.5 algorithm is an extension of the ID3 algorithm and constructs a decision tree to maximize information gain (difference in entropy).
#'
#'@param objetivo Variable a predecir
#'@param predictores Predictores
#'
entrenar_arbol_PART <- function(sets, objetivo, predictores = ".") {
  results <- list()
  
  results[[const$model]] <- PART(formula = generar_formula(objetivo, predictores = predictores), data = sets[[const$train]])
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  
  return (results)
}

## No se puede usar, hace uso de boosting
entrenar_arbol_c50 <- function(sets, objetivo, predictores = ".") {
  results <- list()
  
  results[[const$model]] <- C5.0(formula = generar_formula(objetivo, predictores = predictores), data = sets[[const$train]])
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  
  return (results)
}

entrenar_arbol_tree <- function(sets, objetivo, predictores = ".") {
  results <- list()
  
  results[[const$model]] <- tree(formula = generar_formula(objetivo, predictores = predictores), data = sets[[const$train]])
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  
  return (results)
}

entrenar_arbol_con_postprunning <- function(model, sets, objetivo, predictores = ".", cp = 0.01) {
  
  results <- list()
  results[[const$model]]<-prune(model, cp = cp)
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  
  return (results)
} 

obtener_diagnostico <- function(tree, sets) {
  results <- list()

  results[[const$accuracy]] <- mean(tree[[const$prediction]] == sets[[const$test]]$C)
  results[[const$error]] <- 1-  results[[const$accuracy]]
  
  return (results)
} 

print_diagnostic <- function (trees, sets) {
  results <- list()
  
  results[[const$treeRpart]] <- obtener_diagnostico(trees[[const$treeRpart]], trees[[const$sets]])
  #results[[const$treeC50]] <- obtener_diagnostico(trees[[const$treeC50]], trees[[const$sets]])
  results[[const$treeTree]] <- obtener_diagnostico(trees[[const$treeTree]], trees[[const$sets]])
  results[[const$treePART]] <- obtener_diagnostico(trees[[const$treePART]], trees[[const$sets]])
  
  #results[[const$treeGradienrtBoostedMachine]] <- entrenar_arbol_gradientBoostingMachine(results[[const$sets]], objetivo, predictores)
  #results[[const$treeForest]] <- obtener_diagnostico(trees[[const$treeForest]], trees[[const$sets]])
  #results[[const$treeBagging]] <- obtener_diagnostico(trees[[const$treeBagging]], trees[[const$sets]])
  #results[[const$treeC45]] <- obtener_diagnostico(trees[[const$treeC45]], trees[[const$sets]])
  
  return (results)
}

print_tree <- function(objetivo, dtree) {
  fancyRpartPlot(dtree[[const$model]], main = "Adult Income Level")
}


crear_arbol <- function(datos, objetivo, predictores = ".", withPartitions = TRUE) {
  results <- list()
  
  results[[const$sets]] <- ifelse(withPartitions == TRUE, crear_sets(datos), datos)
  #results[[const$sets]] <- crear_sets(datos)
  results[[const$treeRpart]] <- entrenar_arbol_rpart(results[[const$sets]], objetivo, predictores)
  #results[[const$treeC50]] <- entrenar_arbol_c50(results[[const$sets]], objetivo, predictores)
  results[[const$treeTree]] <- entrenar_arbol_tree(results[[const$sets]], objetivo, predictores) 
  results[[const$treePART]] <- entrenar_arbol_PART(results[[const$sets]], objetivo, predictores)
  #results[[const$treeGradienrtBoostedMachine]] <- entrenar_arbol_gradientBoostingMachine(results[[const$sets]], objetivo, predictores)
  #results[[const$treeForest]] <- entrenar_arbol_randomForest(results[[const$sets]], objetivo, predictores)
  #results[[const$treeBagging]] <- entrenar_arbol_bagging(results[[const$sets]], objetivo, predictores)
  #results[[const$treeC45]] <- entrenar_arbol_c45(results[[const$sets]], objetivo, predictores)
  
  return (results)
}
