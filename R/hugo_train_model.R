#' Training binary classification model and choose the best one
#' 
#' @param data an optional data frame containing the variables in the model
#' @param formula formula describing the model to be fitted
#' 
#' @export
#' @author Anna Kozak
#' @examples
#' \dontrun{
#' data("PimaIndiansDiabetes")
#' formula <- "diabetes~."
#' hugo_train_model(PimaIndiansDiabetes, formula)
#' }
#' 

hugo_train_model <- function(data, formula) {
  .hugoEnv$history[length(.hugoEnv$history) + 1] <- deparse(match.call())
  cat("Hugo starts training models.\n")
  formula <- stats::formula(formula)
  if (length(levels(data[, c(as.character(formula[[2]]))])) != 2) {
    return(
      cat(
        "Response variable has more than two classes. Incorrect data to binary classification.\n"
      )
    )
  }
  if (!is.factor(data[, c(as.character(formula[[2]]))])) {
    data[, c(as.character(formula[[2]]))] <-
      as.factor(data[, c(as.character(formula[[2]]))])
  }
  
  #make a trainControl set
  control <- caret::trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)
  
  
  #regresion model
  glm_model <- caret::train(formula,
                     data = data,
                     method = "glm",
                     trControl = control)
  #random forest model
  randomforest_model <- caret::train(formula,
                              data = data,
                              method = "rf",
                              trControl = control)
  #gbm model
  gbm_model <- caret::train(formula,
                     data = data,
                     method = "gbm",
                     trControl = control)
  #results
  outcome <-
    caret::resamples(
      list(
        glm_model = glm_model,
        randomforest_model = randomforest_model,
        gbm_model = gbm_model
      )
    )
  {
    sum_results <- summary(outcome)
    cat("Statistical variability measured by Accuracy.\n")
    cat(paste(
      "The best model is ",
      sub("\\_.*", "", names(
        which.max(sum_results$statistics$Accuracy[, 4])
      )),
      ".",
      " Accuracy = ",
      round(max(sum_results$statistics$Accuracy[, 4]), 2),
      "\n",
      sep = ""
    ))
    names(which.max(sum_results$statistics$Accuracy[, 4]))
    }
  
  cat("\n")
  result <- names(which.max(sum_results$statistics$Accuracy[, 4]))
  cat("Following variables are related with y.\n")
  cat("\n")
  
  
  
  value <- caret::varImp(get(result))$importance
  number_of_variable <-
    utils::menu(c("Yes", "No"), title = "Hugo shows you the 20 first variables. Do you want see all?\n")
  if (number_of_variable == "No") {
    n <- 20
  } else{
    n <- nrow(value)
  }
  if (result != "model_gbm") {
    row.names(value)[order(-value)]
    caret::varImp(get(result))$importance$Overall[order(-value$Overall)]
    cat(paste(row.names(value)[order(-value$Overall)][1:n],
              paste(
                "(", round(value$Overall[order(-value$Overall)][1:n], 2), ")", sep = ""
              )), sep = ", ")
  } else{
    cat(paste(
      summary(get(result))$var[1:n],
      " ",
      "(",
      round(summary(get(result))$rel.inf[1:n], 2),
      ")",
      ",",
      sep = ""
    ))
    cat("\n")
  }
  return(get(result))
}

  
  
 

