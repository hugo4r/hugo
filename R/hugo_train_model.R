#' Training binary classification model and choose the best one
#' 
#' This function for the given formula and data builds logistic regressions models, 
#' decision trees, gbm model. It saves models to the models catalog and as a result
#' returns trained models and evaluation of how these models are good.
#' 
#' @param data an optional \code{data.frame} containing the variables in the model
#' @param formula formula describing the model to be fitted
#' 
#' @export
#' @author Anna Kozak
#' @return Function returns trained models and assessment of how these models are good.
#' @examples
#' \dontrun{
#' data("PimaIndiansDiabetes")
#' formula <- "diabetes~."
#' hugo_train_model(PimaIndiansDiabetes, formula)
#' }
#' 

hugo_train_model <- function(data = NULL, formula = NULL) {
  
  .hugoEnv$history[length(.hugoEnv$history) + 1] <- deparse(match.call())
  
  if (!file.exists(.hugoEnv$path)) {
    stop(
        "Call hugo_start_investigation() for starting the new investigation."
        )
  }
  if (is.null(data) & is.null(formula)) {
    stop("There are no objects to training.")
  }
  if (is.null(data)) {
    stop("There is no data to training models.")
  }
  if (is.null(formula)) {
    stop("There is no formula to training models.")
  }
  
  path <- paste0(.hugoEnv$path, "/models/")
  if (!dir.exists(path)) {
    dir.create(path)
  }
  
  if (!is.data.frame(data)) {
    stop("Data is not a data.frame.")
  }
  
  {
    formula1 <- gsub(" ", "", formula, fixed = TRUE)
    formula2 <- unlist(strsplit(formula1, "[~]"))
    if (length(formula2) == 2 & formula2[1] %in% names(data)) {
      formula2 <- unlist(strsplit(formula2[2], "[+]"))
      if (!(all(formula2 %in% c(names(data), '.')) & stringi::stri_count_fixed(formula, "+") < length(formula2))) {
        stop("The formula is incorrect.")
      }
    } else {
        stop("The formula is incorrect.")
    }
  }
  
  formula <- stats::formula(formula)
  data <- droplevels(data)
  if (length(levels(data[, c(as.character(formula[[2]]))])) != 2) {
    stop("Response variable has more than two classes. Incorrect data to binary classification.\n")
  }
  if (!is.factor(data[, c(as.character(formula[[2]]))])) {
    data[, c(as.character(formula[[2]]))] <- as.factor(data[, c(as.character(formula[[2]]))])
  }
 
  cat("Hugo starts training models.\n")
  
  #make a trainControl set
  cat("Hugo make a trainControl set: ")
  control <- caret::trainControl(method = "repeatedcv", number = 5, repeats = 3)
  cat("Done. \n")
 
  #regresion model
  cat("Hugo training logistic regression model: ")
  glm_model <- caret::train(formula, data = data, method = "glm", trControl = control)
  cat("Done. \n")
  save(glm_model, file = paste0(path, "glm_model.rda"))
  cat("Hugo save logistic regression model to models catalog.\n") 
 
  #random forest model
  cat("Hugo training random forest model: ")
  randomforest_model <- caret::train(formula, data = data, method = "rf", trControl = control)
  cat("Done. \n")
  save(randomforest_model, file = paste0(path, "randomforest_model.rda"))
  cat("Hugo save random forest model to models catalog.\n")
  
  #gbm model
  cat("Hugo training gbm model: ")
  gbm_model <- caret::train(formula, data = data, method = "gbm", trControl = control)
  cat("Done. \n")
  save(gbm_model, file = paste0(path, "gbm_model.rda"))
  cat("Hugo save gbm model to models catalog.\n")
  
  #results
  outcome <- caret::resamples(list(glm_model = glm_model, randomforest_model = randomforest_model, gbm_model = gbm_model))
  {
    sum_results <- summary(outcome)
    cat("Statistical variability measured by Accuracy.\n")
    cat(paste(
      "The best model is ",
      sub("\\_.*", "", names(which.max(sum_results$statistics$Accuracy[, 4]))), ".", " Accuracy = ", 
      round(max(sum_results$statistics$Accuracy[, 4]), 2), "\n", sep = ""))
    names(which.max(sum_results$statistics$Accuracy[, 4]))
    }
  
  cat("\n")
  result <- names(which.max(sum_results$statistics$Accuracy[, 4]))
  
  #varaibles realated with "y"
  cat(paste("Following variables are related with ", formula[[2]], ".\n", sep = ""))
  cat("\n")
  value <- caret::varImp(get(result))$importance
  if (dim(data)[2] > 7) {
    n <- 7
  } else{
    n <- dim(data)[2] -1
  }
  if (result != "model_gbm") {
    row.names(value)[order(-value)]
    caret::varImp(get(result))$importance$Overall[order(-value$Overall)]
    cat(paste(row.names(value)[order(-value$Overall)][1:n],
              paste("(", round(value$Overall[order(-value$Overall)][1:n], 2), ")", sep = "")), sep = ", ")
    if (n > 7) cat(", ...\n")
  } else {
    cat(paste(summary(get(result))$var[1:n], " ", "(", round(summary(get(result))$rel.inf[1:n], 2), ")", ",", sep = ""))
    if (n > 7) cat(", ...\n")
  }
  
  return(list(logistic_regression = glm_model, randomforest = randomforest_model, gbm = gbm_model, auc = sum_results$statistics$Accuracy))
}

  


