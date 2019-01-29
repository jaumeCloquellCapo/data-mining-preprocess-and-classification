library(dplyr)
library(arules)

iDiscretization <- function (dt, method = "frequency", categories = 3) {
  dt %>% is.numeric %>% discretize(., method=method, categories=categories))
}