#' Saves all important variable and all packages used in project
#' -- There isnt't info about packages implemented yet.
#'
#'
#' @export
#' @author Monika Chudek
#' @examples
#' hugo_save_investigation()

hugo_save_investigation <- function( session_name = NULL){

  obj <- ls(envir = globalenv()) #list all variables saved in environment

  if (!file.exists(.hugoEnv$path)) hugo_start_investigation() # if hugo folder was not initiallized [task]
  resources.path <- paste0(.hugoEnv$path, "/resources")
  if(!file.exists(paste0(.hugoEnv$path, "/resources"))){      # create resourecs if wasnt created yet
    dir.create(resources.path)

  }

  if (is.null(session_name)) session_name = paste0("session", length(saved_sessions)) # set session name

  saved_sessions <- list.files(resources.path)  # list saved sessions
  while(session_name %in% saved_sessions & session_name != 1 ) {
    session_name <- readline(cat('File ', session_name, " exists. Please, enter other name or press 0 to overwrite file" ))
  }

  resources.path <- paste0(resources.path, "/", session_name)  # path to session
  dir.create(resources.path)        # create session directiory

  save( list = obj, file = paste0(resources.path, "/variables"), ascii = FALSE) # save variables
  cat(session_name, " succesfully saved.")


}
