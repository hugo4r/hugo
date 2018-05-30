#' Saves an object as .rda file
#'
#' @param obj name of an object which will be saved as .rda file
#'
#' @export
#' @author Anna Gierlak
#' @examples
#' hugo_memorise()
#'
hugo_memorise <- function(obj) {
  #path <- 'C:/Users/Ania/Desktop/R dla zaawansowanych/projekt3/memory/'
  #path <- '~/Desktop/R/'
  path <- paste(.hugoEnv$path, '/memory/', sep = '')
  if(!dir.exists(path)) {
    dir.create(path)
  }
  file_name <- paste(path, deparse(substitute(obj)), '.rda', sep = '')
  ans <- 0
  if (file.exists(file_name)) {
    cat('Object named \"', deparse(substitute(obj)), '\" already exists in directory ', path, '.\n', sep = '')
    while(ans != 'y' & ans != 'n') {
      ans <- readline(cat('Press:\n\"y\" - if you want to overwrite the object\n\"n\" - otherwise\n'))
      if (ans == 'n') {
        return(cat('The \"', deparse(substitute(obj)), '\" object was not copied.\n', sep = ''))
      } else if (ans != 'y') {
        cat('Wrong answer, try again!\n')
      }
    }
  }
  
  obj_var <- setNames(as.list(obj), deparse(substitute(obj)))
  save(list = names(obj_var), file = file_name, envir = as.environment(obj_var))

  cat('Copy of the \"', deparse(substitute(obj)), '\" object is stored in ', path, '.\n', sep = '')

}




