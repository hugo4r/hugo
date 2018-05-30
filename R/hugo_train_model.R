#' Starts a New Hugo Investigation
#'
#' @param data data to train and test model
#' @param formula formula for model
#'
#' @export
#' @author Anna Kozak
#' @examples
#' hugo_train_model()
#'


 
hugo_train_model <- function(data, formula){
  # wyzaczenie zbioru treningowego - domyslnie boot
  df.train <- trainControl(method = "boot", number = 10)
  
  ### pozostale metody: "boot", "boot632", "optimism_boot", "boot_all", "cv", "repeatedcv", "LOOCV", "LGOCV"
  
  
  # model regresyjny
  model.lm <- train(formula,  method="lm")
  
  # model drzew decyzyjnych
  model.part <- train(formula, df.train, method = "rpart")
  
  # gbmlight
  # pakiet lightgbm?
  
  
  #ocena klasyfikatora - pakiet caret - trainControl(method="boot") - domyslnie
  
  
  #otrzymany wynik
  #mozemy zapytac uzytkownika czy chce zmienic metode oceny klasyfikatora
}
