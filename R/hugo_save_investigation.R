#' Saves all important variables and all packages used in project
#' -- There istn't info about packages implemented yet.
#'
#'
#' @export
#' @author Monika Chudek
#' @examples
#' hugo_save_investigation()

hugo_save_investigation <- function(){

  if (!file.exists(.hugoEnv$path)) hugo_start_investigation()

  if(!file.exists(paste0(.hugoEnv$path, "/resources"))){
    resources.path <- paste0(.hugoEnv$path, "/resources")
    dir.create(resources.path)
  }
    obj <- paste(ls(envir = globalenv()), collapse = ", ")
    save.image(file = paste0(.hugoEnv$path, "/resources/variables"))
    if(obj!="") cat(paste("following objects are stored: ", obj))
    else cat("no variable to save")
}
