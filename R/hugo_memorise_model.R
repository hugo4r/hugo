#' Saves a model
#'
#' Saves a model in the directory chosen at the beginning of the current
#' Hugo Investigation in the subfolder 'gallery' in .rda file. In addition, the function
#' saves a summary in .md file based on tidy data frames from package \code{broom}.
#' Be aware that it can be done only for models supported by \code{broom}.
#'
#'
#' @param model a model to be saved
#' @param name a name of files
#'
#' @export
#' @author Malgorzata Lazecka
#' @examples
#' \dontrun{mod.lm <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' hugo_memorise_model(mod.lm, name = "model.iris")
#' # Or save as "mod.lm":
#' hugo_memorise_model(mod.lm)}

hugo_memorise_model <- function(model = NULL, name = substitute(model)) {

  .hugoEnv$history[length(.hugoEnv$history) + 1] <- deparse(match.call())

  if (!file.exists(.hugoEnv$path)) {
    stop('Call hugo_start_investigation() for starting the new investigation.')
  }

  if (is.null(model)) {
    stop("There is no model to be saved.", call.=FALSE)
  }

  path <- paste0(.hugoEnv$path, "/gallery")
  if (!dir.exists(path)) {
    dir.create(path)
  }

  replace_output <- NULL

  if(file.exists(paste0(path, "/", name, ".rda"))) {
    replace_output <- switch(utils::menu(choices = c("TRUE", "FALSE"), title = paste0("Do you want to replace existing report with the same name: >", name, "?")), TRUE, FALSE)
  }

  if (is.null(replace_output)) {
    replace_output <- TRUE
    message("Default value will be used: TRUE")
  }

  if (!replace_output) {
    stop(paste0("Model ", name," already saved."))
  }

  save(model, file = paste0(path, "/", name, ".rda"))

  tryCatch(expr = {
    rfile <- file(paste0(path, "/", name, ".md"), open = "wt")
    sink(rfile, type = "output")
    cat("Test results:", "\n")
    print(broom::tidy(model))
    cat("\n", "Augment data:", "\n")
    print(broom::augment(model))
    cat("\n", "Single row summary:", "\n")
    print(broom::glance(model))
    sink(type="output")
    close(rfile)
  }, warning = function(warning_communicate) {
    sink(type="output")
    close(rfile)
    file.remove(paste0(path, "/", name, ".md"))
    warning("From package \"broom\" while creating a summary: ", warning_communicate)
  }, error = function(error_communicate) {
    sink(type = "output")
    close(rfile)
    file.remove(paste0(path, "/", name, ".md"))
    error_communicate$message <- paste0("From package \"broom\" while creating a summary: ", error_communicate)
    stop(error_communicate)
  })

  add_path_to_history(paste0(path, "/", name, ".md"))
  add_path_to_history(paste0(path, "/", name, ".rda"))
  cat("Model ", name, " and summary saved in ", path, "\n")
}

