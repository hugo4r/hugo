#' Continue Hugo investigation
#' @author Mateusz Kobylka
#'
#' @description Loads all important variables and all packages from saved investigation.
#'The investigation can be saved with  \code{hugo_save_investigation} function.
#'
#'
#' @usage hugo_continue_investigation(path = NULL, session_name = NULL, envir = .GlobalEnv)
#'
#' @param path name of a directory in which the investigation is stored
#' @param session_name name of a session to be restored
#' @param envir an environment to load variables. Global Environment by default.
#'
#' @details Each investigation can consist of multiple sessions and thus
#' if \code{session_name} is not provided, the function either loads the data if only one session exists or asks user
#' to type the session name which is going to be restored. The data is loaded from '\code{resources/{session_name}}' folder, which
#' contains two files: \code{variables} and \code{packages}. The first one keeps all important variables, while
#' the second one is used to load all packages which were attached during the saved investigation. The function
#' tries to load compatible versions of packages, or, if it is not possible, installs latest versions of troublesome packages.
#'
#' @seealso \code{\link{hugo_save_investigation}}, \code{\link{hugo_start_investigation}}
#'
#' @examples
#' hugo_start_investigation("iris")
#' ## New investigation started in  iris
#' my_boxplot <- boxplot(iris$Sepal.Length)
#' hugo_save_investigation("iris_session1")
#' ## iris_session1  succesfully saved.
#' remove(my_boxplot)
#' hugo_continue_investigation("iris")
#' ## Loading required packages to continue investigation ...
#' ## Session iris_session1 from investigation iris sucessfully restored.
#' @export


hugo_continue_investigation <- function(path = NULL, session_name = NULL, envir = .GlobalEnv) {


  .hugoEnv$history[length(.hugoEnv$history)+1] <- deparse(match.call()) # Add function call to history

  if (is.null(path)) {
    path <- .hugoEnv$path
  } else  {
    .hugoEnv$path <- path
  }

  resources_path <- paste0(.hugoEnv$path, "/resources")
  saved_sessions <- list.dirs(resources_path, recursive = FALSE, full.names = FALSE)

  if(is.null(session_name)){ # Check available sessions
    if(length(saved_sessions) >1 )
      {
        cat("I've found multiple sessions in the specified path. Which session should I continue?\n")
        session_name <- readLines(con = getOption('hugo.connection_in'),n=1)

      } else {

        session_name <- saved_sessions[1] # Load session if only one exists
      }
  }

  session_path <- paste0(resources_path,"/",saved_sessions[saved_sessions == session_name])

  #Try loading file with packages' versions
  packages <- tryCatch(read_packages("packages",session_path),
                       warning = function(w) { cat("There are no saved sessions ",
                                               ifelse(is.na(session_name),"",session_name),
                                               " in the specified path.\n", sep="")
                                              })

  #Stop if there is nothing to be restored
  if (is.null(packages)) stop("I cannot restore the investigation.\n")

  #Check if packages' versions are compatible
  cat("Loading required packages to continue investigation ... \n")
  existing_packages <- data.frame(utils::installed.packages())
  existing_packages <- existing_packages[,c("Package","Version")]
  not_attached <- integer(0)

  for(i in 1:length(packages$Package)) {
      if(nrow(merge(packages[i,],existing_packages))>0 ) #Load package if the versions are compatible
        {
          library(package = packages[i,1], character.only = TRUE)
        } else
        {
          tryCatch({ #Try installing version saved in investigation
                    cat("I'm installing",packages[i,2],"version of",packages[i,1],"package.\n")
                    devtools::install_version(package = packages[i,1], version = packages[i,2])
                    library(package = packages[i,1], character.only = TRUE)
                   },
                   error = function(e) {cat("I cannot install",packages[i,2],"version of",packages[i,1],"package.\n")
                                        not_attached <<- c(not_attached,i)}) #Save packages which Hugo was not able to install
        }
  }

  if(length(not_attached)>0) #Last chance: try installing latest versions of not installed packages
  {
    packages <- packages[not_attached,]
    cat("I was not able to install following packages: ", paste(packages$Package,packages$Version),"\n")
    cat("Trying to install latest versions of packages ... \n")
    for (i in 1:length(not_attached)) {
      tryCatch( {utils::install.packages(packages[i,1])
                 library(package = packages[i,1], character.only = TRUE)},
                  error = function(e) {cat("I was not able to install",packages[i,1])})
      }
    cat("I will load the investigation, but you might need to install not attached packages manually in order to continue your analysis.\n")

  }

  #Load variables and print information

  load(paste0(session_path,"/variables"),envir = envir)
  cat("Session",session_name,"from investigation", path, "sucessfully restored.")

}

#Reads packages and their versions from session_info file. Uses regular expressions.
read_packages <- function(file,path) {

  lines <- readLines(paste(path,file,sep="/"))
  package_line <- which.max(sapply(lines, grep, pattern = "package " ))+1
  lines <- strsplit(lines[package_line:length(lines)], split="\\s+")
  package_name <- sapply(lines, "[[", 2)
  package_version <- sapply(lines, stringr::str_extract, pattern = "(\\d+[\\.\\-])+\\d+")
  package_version <- sapply(sapply(package_version,stats::na.omit),"[[",1)
  packages <- data.frame(Package=package_name,Version=package_version)
  packages$Package <- as.character(packages$Package)
  packages$Version <- as.character(packages$Version)
  packages
}
