#' Save variables and information about packages
#'
#' @description The \code{hugo_save_investigation} saves variables and versions of packages used in investigation.
#' Data is stored in subfolder \code{resources/session_name} in two separate files: \code{variables} and \code{packages}.
#' Restoring data and loading saved versions of packages could be done by using function \code{\link{_hugo_continue_investigation_}}.
#'
#' @param session_name name of directory in which the session will be saved.
#' If session_name = NULL, the dafault name is set.
#' @param variables a character vector containing names of important variables to be saved.
#' By default \code{variables = NULL}, what means that all the variables in environment \code{envir} are saved.
#'
#' @param envir an environment to search for objects to be saved. Global Environment by default.
#'
#' @export
#' @author Monika Chudek
#' @examples
#' \dontrun{hugo_start_investigation("Example")
#' model <- lm(Petal.Width~., data = iris)
#' res <- model$residuals
#' petal.pred <- predict(model, newdata = iris)
#' hugo_save_investigation()
#'
#' # or select variables to be written out and set name of session's directory:
#' hugo_save_investigation( variables = c('petal.pred', 'res'), session_name = 'IrisModel')
#'
#' # or select the environment that contains variables for saving
#' e <- new.env()
#' e$a <- FALSE
#' e$b <- "a"
#' expect_output(hugo_save_investigation(envir = e))
#' }

hugo_save_investigation <- function( variables = NULL, session_name = NULL, envir = .GlobalEnv){

  #add_to_history("hugo_save_investigation")
  if (!file.exists(.hugoEnv$path)) {                                # if investigation wasn't called
    cat("hugo_start_investigation() wasn't called.\n Enter investigation's path or 0 to continue with default path parameter.\n")
    ans <- readLines(con = getOption("hugo.connection_in"), n = 1)
    if(ans == "0") hugo_start_investigation()
    else hugo_start_investigation(ans)
  }

  if(length(variables) != 0) {                                                          # if user entered variables
    obj <- variables[!variables %in% ls(envir = envir)]                                 # chceck if all of them are in global env
    if(length(obj) != 0)                                                                # if variables are not defined in env
      stop( paste('Following varables are not defined:', paste(obj, collapse = ", ")))
    obj <- variables
  } else {
    obj <- ls(envir = envir)
  }

  if (length(obj) == 0)  warning("No variables to save.")                               # stop('No variables to save.')

  resources.path <- paste0(.hugoEnv$path, "/resources")
  if(!file.exists(paste0(.hugoEnv$path, "/resources")))   dir.create(resources.path)    # create resourecs dir if it wasnt created yet

  saved_sessions <- list.files(resources.path)  # list saved sessions
  if (is.null(session_name)) session_name = paste0("session", length(saved_sessions))   # set session name

  answ <- 1
  while(session_name %in% saved_sessions & answ != "0" ) {
    cat('Session ', session_name, " already exists. Please, enter other name or press 0 to overwrite the file" )
    answ <- readLines(con = getOption("hugo.connection_in"), n = 1)
    if(answ != 0 ) session_name <- answ
  }

  resources.path <- paste0(resources.path, "/", session_name)                          # path to session
  dir.create(resources.path)                                                           # create session dir

  save( list = obj, file = paste0(resources.path, "/variables"), ascii = FALSE, envir = envir) # save variables

  sfile <- file(paste0(resources.path, "/packages"), open="wt")                        # save packages
  sink(sfile)
  sink(sfile, type = "message")
  print(devtools::session_info())
  sink(type = "message")
  sink()

  cat(session_name, " succesfully saved.\n")
}
