#' Share and download object from github.
#'
#' To use both of this functions, it is required to have git installed.
#' Becouse of git2r library limitations, those functions won't work if either
#' user.name or user.email are empty in git config file. Uses .rds format to save objects.
#' Hugo_get_object is intended to use only with hugo_share_object - it is not general
#' function to download object from github, and don't try to use it as one.
#'
#' While using hugo_share_object user is prompted and asked to insert additional parameters - github user name, password for given user, and repository name, to wich you want to add your object.
#' If hugo remebers previus answer, user will be prompted and asked if he wishes to use current
#' credentials or insert new ones.
#'
#' @describeIn  hugo_share_object Shares given object via github, prompts user for input.
#'
#'    Returns: in case of success prompts user with message, and returns string, which is
#'    direct call to hugo_get_object. In case of failure, prompts user with appropriate
#'    message and returns NULL.
#'
#' @param object R object which you wish to share.
#' @param user Github user name, to construct proper url to repository.
#' @param repo_name Name of github repository, also used to construct url.
#' @param object_name Name of the object you want to download.
#' @return Please check Functions section.
#' @examples
#' \dontrun{
#' hugo_share_object(iris)
#' hugo_get_object(matrix(runif(200), ncol = 10))
#'
#' object_1 <- hugo_get_object('tidyverse','readr','revdep/checks.rds')
#' object_2 <- hugo_get_object('hadley','r-python','github.rds')
#'}
#' @export



hugo_share_object <- function(object) {

  if (!requireNamespace("git2r", quietly = TRUE)) {
    stop("You have to first install library 'git2r'. ", call. = FALSE)
  }

  if (!requireNamespace("getPass", quietly = TRUE)) {
    stop("You have to first install library 'getPass'. ", call. = FALSE)
  }

  if(!getOption("hugo.know_credentials")) {

  cat("Please insert your github user name: > ",file = getOption('hugo.connection_out'))
  user <- readLines(con = getOption('hugo.connection_in'), n = 1)
  options(hugo.user_name = user)

  if(getOption('hugo.connection_out') != stdout() | !interactive()){
  cat(paste0("Please insert password for user ", getOption('hugo.user_name'), ": > ", sep = " "),
      file = getOption('hugo.connection_out'))
  pass <-  readLines(con = getOption('hugo.connection_in'), n = 1)
  } else {
    pass <- getPass::getPass(paste0("Please insert password for user ",
          getOption('hugo.user_name'), ": > ", sep = " "))

  }

  cat("Please insert github repository name: > ",file = getOption('hugo.connection_out'))
  repo_name <-  readLines(con = getOption('hugo.connection_in'), n = 1)
  options(hugo.repo_name = repo_name)


  } else {

    chosen_input <- switch(utils::menu(c('Use previous settings.','Enter new credentials.'),
                     title = paste0("Found previous settings: user = ",
                     getOption('hugo.user_name'), ' , repo_name = ',
                     getOption('hugo.repo_name'), '.')),T,F)
    if(is.null(chosen_input)) {return()}
    if(chosen_input) {
      if(getOption('hugo.connection_out') != stdout() | !interactive()){
        cat(paste0("Please insert password for user ", getOption('hugo.user_name'), ": > ", sep = " "),
            file = getOption('hugo.connection_out'))
        pass <-  readLines(con = getOption('hugo.connection_in'), n = 1)
      } else {
        pass <- getPass::getPass(paste0("Please insert password for user ",
              getOption('hugo.user_name'), ": > ", sep = " "))

      }
    } else {

      cat("Please insert your github user name: > ",file = getOption('hugo.connection_out'))
      user <- readLines(con = getOption('hugo.connection_in'), n = 1)
      options(hugo.user_name = user)

      if(getOption('hugo.connection_out') != stdout() | !interactive()){
        cat(paste0("Please insert password for user ", getOption('hugo.user_name'), ": > ", sep = " "),
            file = getOption('hugo.connection_out'))
        pass <-  readLines(con = getOption('hugo.connection_in'), n = 1)
      } else {
        pass <- getPass::getPass(paste0("Please insert password for user ",
              getOption('hugo.user_name'), ": > ", sep = " "))

      }

      cat("Please insert github repository name: > ",file = getOption('hugo.connection_out'))
      repo_name <-  readLines(con = getOption('hugo.connection_in'), n = 1)
      options(hugo.repo_name = repo_name)
      }
  }

  options(hugo.know_credentials = T)

  cat("I`m trying to share your object... \n",file = getOption('hugo.connection_out'))

  tryCatch(expr = {
    path <- tempfile(pattern = paste0("git2r-",
        getOption('hugo.user_name'), "-", getOption('hugo.repo_name'), "-"))
    dir.create(path)
    repo <- git2r::clone(paste0("https://github.com/",
        getOption('hugo.user_name'), "/", getOption('hugo.repo_name'), ".git"),
                         path, progress = F)
  }, error = function(e) {
    message("Failed while connecting with given repository.")
    message("Here's the original error message:")
    message(e)
  })

  if (!exists("repo", environment(), inherits = F)) {
    return()
  }

  tryCatch(expr = {
    object_name <- paste0("hugo_shared_object_", gsub(" |:|-", "_", Sys.time()), ".rds")
    saveRDS(object, file.path(path, object_name))
    git2r::add(repo, file.path(path, object_name))
    git2r::commit(repo, object_name)
    git2r::push(repo, credentials = git2r::cred_user_pass(
      getOption('hugo.user_name'), pass))
    succ = T
  }, error = function(e) {
    message("Failed while adding object to given repository.")
    message("Here's the original error message:")
    message(e)
  })

  if (!exists("succ", environment(), inherits = F)) {
    return()
  }

  message("Success!")
  return(paste0("hugo_get_object('", getOption('hugo.user_name'),
        "','", getOption('hugo.repo_name'), "','", object_name, "')"))
}





#' @describeIn  hugo_share_object Downloads given object from github.
#'    Returns: in case of success prompts user with message, and returns object.
#'    It us up to user how to handle it - wheter to assign it to variable,
#'    or use as parameter. In case of failure, prompts user with appropriate
#'    message and returns NULL.
#'
#' @export



hugo_get_object <- function(user, repo_name, object_name) {

  if (!requireNamespace("git2r", quietly = TRUE)) {
    stop("You have to first install library 'git2r'. ", call. = FALSE)
  }

  path <- tempfile(pattern = paste0("git2r-", user, "-", repo_name, "-"))
  dir.create(path)

  cat("I`m trying to get your object... \n",file = getOption('hugo.connection_out'))

  tryCatch(expr =
             repo <- git2r::clone(paste0("https://github.com/", user, "/", repo_name, ".git"),
                                  path, progress = F),
           error = function(e) {
             message("Failed while connecting with given repository.")
             message("Here's the original error message:")
             message(e)
           })

  if (!exists("repo", environment(), inherits = F)) {
    return()
  }

  tryCatch(expr = {
    object <- suppressWarnings(readRDS(file.path(path, paste0(object_name))))
    succ = T
  }, error = function(e) {
    message("Failed while loading object from given repository.")
    message("Here's the original error message:")
    message(e)
  })

  if (!exists("succ", environment(), inherits = F)) {
    return()
  }

  message("Success!")
  return(object)
}




