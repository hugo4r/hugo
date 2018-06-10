#' Saving objects as .rda files
#'
#' The \code{hugo_memorise} function saves an object as .rda file in the memory subdirectory.
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
#' hugo_memorise(list('abcdef', seq(0,5,0.1), c(TRUE,FALSE)))
#' }
hugo_memorise <- function(object = NULL) {

  add_to_history('hugo_memorise')

  if (!file.exists(.hugoEnv$path)) {
    stop('Call hugo_start_investigation() for starting the new investigation.')
  }

  if (is.null(object)) {
    stop('There is no object to be saved.', call. = FALSE)
  }

  path <- paste0(.hugoEnv$path, '/memory/')
  if(!dir.exists(path)) {
    dir.create(path)
  }
  file_name <- paste0(path, deparse(substitute(object)), '.rda')

  ans <- -1
  if (file.exists(file_name)) {
    cat('Object named \"', deparse(substitute(object)), '\" already exists in directory ', path, '.\nDo you want to overwrite it?\n', sep = '')
    while(ans != 0 & ans != 1) {
      ans <- readline(cat('1: Yes\n0: No\n'))
      if (ans == 0) {
        return(cat('The \"', deparse(substitute(object)), '\" object was not copied.\n', sep=''))
      } else if (ans != 1) {
        cat('Wrong answer, try again!\n')
      }
    }
  }

  object_var <- stats::setNames(as.list(object), deparse(substitute(object)))
  save(list = names(object_var), file = file_name, envir = as.environment(object_var))
  cat('Copy of the \"', deparse(substitute(object)), '\" object is stored in ', path, '.\n', sep='')

}

