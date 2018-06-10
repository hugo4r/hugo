#' Saves a model
#'
#' Saves a model in the directory chosen at the beginning of the current
#' Hugo Investigation in the subfolder 'gallery' in .rda file and a summary in .md file
#'
#' @param model a model to be saved
#' @param name a name of files
#'
#' @export
#' @author Malgosia Lazecka
#' @examples
#' \dontrun{mod.lm <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' hugo_memorise_model(mod.lm, name = "model.iris")
#' # Or save as "mod.lm":
#' hugo_memorise_model(mod.lm)}

hugo_memorise_model <- function(model = NULL, name = substitute(model)) {

  # add_to_history("hugo_memorise_model")

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

  tryCatch({
    save(model, file=paste0(path, "/", name,".rda"))
  }, warning = function(w) {
    warning("While saving a model: ", w)
  }, error = function(e) {
    e$message <- paste0("While saving a model: ", e)
    stop(e)
  })

  tryCatch({
    rfile <- file(paste0(path, "/", name, ".md"), open="wt")
    sink(rfile, type="output")
    cat("Test results:", "\n")
    print(broom::tidy(model))
    cat("\n", "Augment data:", "\n")
    print(broom::augment(model))
    cat("\n", "Single row summary:", "\n")
    print(broom::glance(model))
  }, warning = function(w) {
    warning("From package \"broom\" while creating a summary: ", w)
  }, error = function(e) {
    e$message <- paste0("From package \"broom\" while creating a summary: ", e)
    stop(e)
  }, finally = {
    sink(type="output")
    close(rfile)
  })

  cat("Model ", name, " and summary saved in ", path, "\n")
}

