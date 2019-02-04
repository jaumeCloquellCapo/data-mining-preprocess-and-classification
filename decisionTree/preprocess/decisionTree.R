library(tree)
library(rpart)
library(rstudioapi)
library(tree)
library(rJava)
library(partykit)
library(dplyr)
library(party)
library(caret)
library(C50)

const <- list(
  "train" = "train",
  "test"    = "test",
  "prediction" = "prediction",
  "prunedPrediction" = "prunedPrediction",
  "class" = "class",
  "treeRpart" = "treeRpart",
  "treeTree" = "treeTree",
  "treeC50" = "treeC50",
  "treeC45" = "treeC45",
  "treeGradienrtBoostedMachine" = "treeGradienrtBoostedMachine",
  "treeForest" = "treeForest",
  "treeBagging" = "treeBagging",
  "model" = "model",
  "referencia" = "referencia",
  "sets" = "sets",
  "diagnostic" = "diagnostic",
  "postPrunedModel" = "postprunedModel",
  "prePrunedModel" = "preprunedModel",
  "accuracy" = "accuracy",
  "error" = "error"
)

crear_sets <- function(datos, proporcion = .7) {
  results <- list()
  
  results[[const$train]] <- sample_frac(datos, proporcion)
  results[[const$test]] <- setdiff(datos, results[[const$train]])
  
  return (results)
}

generar_formula <- function(objetivo, predictores = ".") {
  if(length(predictores > 1)) {
    predictores <- paste0(predictores, collapse = "+")
  }
  formula <- paste0(objetivo, " ~ ", predictores) %>% as.formula()
  return (formula)
}

entrenar_arbol_rpart <- function(sets, objetivo, predictores = ".") {
  results <- list()
  
  results[[const$model]] <- rpart(data = sets[[const$train]], formula = generar_formula(objetivo, predictores = predictores))
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  #results[[const$diagnostic]] <- obtener_diagnostico(result[[const$prediction]], sets[[const$test]])
  
  return (results)
}

entrenar_arbol_c45 <- function(sets, objetivo, predictores = ".") {
  results <- list()
  
  results[[const$model]] <- J48(formula = generar_formula(objetivo, predictores = predictores), data = sets[[const$train]])
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  #results[[const$diagnostic]] <- obtener_diagnostico(result[[const$prediction]], sets[[const$test]])
  
  return (results)
}

entrenar_arbol_bagging <- function(sets, objetivo, predictores = ".") {
  results <- list()
  
  results[[const$model]] <- bagging(formula = generar_formula(objetivo, predictores = predictores), data = sets[[const$train]])
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  #results[[const$diagnostic]] <- obtener_diagnostico(result[[const$prediction]], sets[[const$test]])
  
  return (results)
}

entrenar_arbol_randomForest <- function(sets, objetivo, predictores = ".") {
  results <- list()
  
  results[[const$model]] <- randomForest(formula = generar_formula(objetivo, predictores = predictores), data = sets[[const$train]], na.action=na.omit)
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  #results[[const$diagnostic]] <- obtener_diagnostico(result[[const$prediction]], sets[[const$test]])
  
  return (results)
}

entrenar_arbol_gradientBoostingMachine <- function(sets, objetivo, predictores = ".") {
  results <- list()
  
  results[[const$model]] <- gbm(formula = generar_formula(objetivo, predictores = predictores), data = sets[[const$train]], n.trees = 100)
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  #results[[const$diagnostic]] <- obtener_diagnostico(result[[const$prediction]], sets[[const$test]])
  
  return (results)
}

entrenar_arbol_c50 <- function(sets, objetivo, predictores = ".") {
  results <- list()
  
  results[[const$model]] <- C5.0(formula = generar_formula(objetivo, predictores = predictores), data = sets[[const$train]])
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  #results[[const$diagnostic]] <- obtener_diagnostico(result[[const$prediction]], sets[[const$test]])
  
  return (results)
}

entrenar_arbol_tree <- function(sets, objetivo, predictores = ".") {
  results <- list()
  
  results[[const$model]] <- tree(formula = generar_formula(objetivo, predictores = predictores), data = sets[[const$train]])
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  #results[[const$diagnostic]] <- obtener_diagnostico(result, sets[[const$test]])
  
  return (results)
}

entrenar_arbol_con_postprunning <- function(model, sets, objetivo, predictores = ".", cp = 0.01) {
  
  results <- list()
  results[[const$model]]<-prune(model, cp = cp)
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  #results[[const$referencia]] <- sets[[const$test]][[objetivo]]
  
  return (results)
} 

entrenar_arbol_con_preprunning <- function(sets, objetivo, predictores = ".", cp = 0) {
  
  
  results <- list()
  results[[const$model]] <- rpart(data = sets[[const$train]], formula = generar_formula(objetivo, predictores = predictores), control = rpart.control(cp = cp, maxdepth = 8,minsplit = 100))
  results[[const$prediction]] <- predict(results[[const$model]], sets[[const$test]], type = "class")
  results[[const$referencia]] <- sets[[const$test]][[objetivo]]
  
  return (results)
} 

obtener_diagnostico <- function(tree, sets) {
  results <- list()
  
  results[[const$accuracy]] <- mean(tree[[const$prediction]] == sets[[const$test]]$C)
  #results[["table"]] <- table(tree[[const$prediction]] == sets[[const$test]]$C)
  #results[[const$error]] <- min(sum(labels == 1), sum(labels == 0))
  
  return (results)
} 

print_diagnostic <- function (trees, sets) {
  results <- list()
  
  results[[const$treeRpart]] <- obtener_diagnostico(trees[[const$treeRpart]], trees[[const$sets]])
  results[[const$treeC50]] <- obtener_diagnostico(trees[[const$treeC50]], trees[[const$sets]])
  results[[const$treeTree]] <- obtener_diagnostico(trees[[const$treeTree]], trees[[const$sets]])
  
  #results[[const$treeGradienrtBoostedMachine]] <- entrenar_arbol_gradientBoostingMachine(results[[const$sets]], objetivo, predictores)
  results[[const$treeForest]] <- obtener_diagnostico(trees[[const$treeForest]], trees[[const$sets]])
  results[[const$treeBagging]] <- obtener_diagnostico(trees[[const$treeBagging]], trees[[const$sets]])
  results[[const$treeC45]] <- obtener_diagnostico(trees[[const$treeC45]], trees[[const$sets]])
  
  return (results)
}

print_tree <- function(objetivo, dtree) {
  fancyRpartPlot(dtree[[const$model]], main = "Adult Income Level")
}


crear_arbol <- function(datos, objetivo, predictores = ".", withPartitions = TRUE) {
  results <- list()
  
  results[[const$sets]] <- ifelse(withPartitions == TRUE,crear_sets(datos), datos)
  results[[const$sets]] <- crear_sets(datos)
  results[[const$treeRpart]] <- entrenar_arbol_rpart(results[[const$sets]], objetivo, predictores)
  results[[const$treeC50]] <- entrenar_arbol_c50(results[[const$sets]], objetivo, predictores)
  results[[const$treeTree]] <- entrenar_arbol_tree(results[[const$sets]], objetivo, predictores)
  
  #results[[const$treeGradienrtBoostedMachine]] <- entrenar_arbol_gradientBoostingMachine(results[[const$sets]], objetivo, predictores)
  results[[const$treeForest]] <- entrenar_arbol_randomForest(results[[const$sets]], objetivo, predictores)
  results[[const$treeBagging]] <- entrenar_arbol_bagging(results[[const$sets]], objetivo, predictores)
  results[[const$treeC45]] <- entrenar_arbol_c45(results[[const$sets]], objetivo, predictores)
  
  return (results)
}
