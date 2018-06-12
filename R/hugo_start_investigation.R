#' Starts a New Hugo Investigation
#'
#' @param path name of a directory in which the investigation will be stored
#'
#' @export
#' @author Przemyslaw Biecek
#' @examples
#' hugo_start_investigation()
#'
hugo_start_investigation <- function(path = NULL) {
  .hugoEnv$history[length(.hugoEnv$history) + 1] <- deparse(match.call())
  if (is.null(path)) {
    path <- .hugoEnv$path
  } else  {
    .hugoEnv$path <- path
  }

  if (dir.exists(path)) {
    cat("Investigation ", path, " already exists\n")
  } else {
    dir.create(path)
    sfile <- file(paste0(path, "/.session_info"), open="wt")
    sink(sfile)
    sink(sfile, type = "message")
    print(devtools::session_info())
    sink(type = "message")
    sink()
    cat("New investigation started in ", path, "\n")
  }

}
