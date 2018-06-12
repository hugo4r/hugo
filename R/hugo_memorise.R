#' Saving objects as .rda files
#'
#' The \code{hugo_memorise} function saves an object as .rda file in the 'memory' subdirectory.
#' If the object of the same name has been already saved, it gives the opportunity either
#' to overwrite it or to stop the function execution.
#'
#' @param object an object to be saved as .rda file
#'
#' @export
#' @author Anna Gierlak
#'
#' @examples
#' \dontrun{
#' a <- 1:5
#' hugo_memorise(a)
#'
#' b <- list('abcdef', seq(0,5,0.1), c(TRUE,FALSE))
#' hugo_memorise(b)
#' }
hugo_memorise <- function(object = NULL) {

  .hugoEnv$history[length(.hugoEnv$history)+1]<-deparse(match.call())

  if (!file.exists(.hugoEnv$path)) {
    stop('Call hugo_start_investigation() for starting the new investigation.')
  }

  if (is.null(object)) {
    stop('There is no object to be saved.')
  }

  path <- paste0(.hugoEnv$path, '/memory/')
  if(!dir.exists(path)) {
    dir.create(path)
  }
  file_name <- paste0(path, deparse(substitute(object)), '.rda')

  if (file.exists(file_name)) {
    cat('Object named \"', deparse(substitute(object)), '\" already exists in directory ', path, '.\nDo you want to overwrite it?\n', sep = '')
    cat('Yes: 1\nNo: 2\n')
    ans <- 0
    while(ans == 0) {
      ans <- readLines(con = getOption('hugo.connection_in'), n = 1)
      if (ans == 2) {
        return(cat('The \"', deparse(substitute(object)), '\" object was not copied.\n', sep=''))
      } else if(ans != 1) {
        cat('Wrong answer, try again!\n')
        ans <- 0
      }
    }
  }

  object_var <- stats::setNames(as.list(object), deparse(substitute(object)))
  save(list = names(object_var), file = file_name, envir = as.environment(object_var))
  add_path_to_history(file_name)
  cat('Copy of the \"', deparse(substitute(object)), '\" object is stored in ', path, '.\n', sep='')

}


