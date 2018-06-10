#' Save variables and information about packages
#'
#' @description hugo_save_investigation writes variables and packages used in session.
#' Written data can be read back at later date by using function hugo_continue_investigation.
#'
#' @param session_name name of directory in which the session will be saved.
#' If session_name = NULL, the dafault name is set.
#' @param variables A character vector containing the names of objects to be saved.
#' If variables = NULL, all variables in Global Environment are saved.
#'
#' @export
#' @author Monika Chudek
#' @examples
#' \dontrun{hugo_start_investigation("Example")
#' model <- lm(Petal.Width~., data = iris)
#' res <- model$residuals
#' petal.pred <- predict(model, newdata = iris)
#' hugo_save_investigation()
#' # Or select variables to be written out and set name of session's directory:
#' hugo_save_investigation( variables = c('petal.pred', 'res'), session_name = 'IrisModel')}

hugo_save_investigation <- function( variables = NULL, session_name = NULL){

  add_to_history("hugo_save_investigation")

  if (!file.exists(.hugoEnv$path))
    stop('Call hugo_start_investigation() for statring the new investigation')# if hugo folder was not initiallized


  if(length(variables) != 0){     # if user entered variables
    obj <- variables[!variables %in% ls(envir = globalenv())] # chceck if all of them are in global env
    if(length(obj) != 0)
      stop( paste('Following varables are not defined:', paste(obj, collapse = ", ")))
    obj <- variables
  } else{
    obj <- ls(envir = globalenv())
  }

  if (length(obj) == 0)
    stop('No variables to save.')

  resources.path <- paste0(.hugoEnv$path, "/resources")
  if(!file.exists(paste0(.hugoEnv$path, "/resources")))   dir.create(resources.path)    # create resourecs dir if it wasnt created yet

  saved_sessions <- list.files(resources.path)  # list saved sessions
  if (is.null(session_name)) session_name = paste0("session", length(saved_sessions)) # set session name

  ans <- 1
  while(session_name %in% saved_sessions & ans != 0 ) {
    ans <- readline(cat('Session ', session_name, " already exists. Please, enter other name or press 0 to overwrite the file" ))
    if(ans != 0 ) session_name <- ans
  }

  resources.path <- paste0(resources.path, "/", session_name)  # path to session
  dir.create(resources.path)        # create session dir

  save( list = obj, file = paste0(resources.path, "/variables"), ascii = FALSE, overwrite = T) # save variables


  sfile <- file(paste0(resources.path, "/packages"), open="wt")                 # save packages
  sink(sfile)
  devtools::session_info()
  sink()

  cat(session_name, " succesfully saved.")
  closeAllConnections()
}
