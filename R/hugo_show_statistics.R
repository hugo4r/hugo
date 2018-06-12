#'
#' Gives the brief overview of the present hugo statistics.
#'
#'
#' hugo_show_statistics() is a generic function used to produce
#' summaries of hugo investigation.
#'
#' @return Function returns a list containing following components:
#' \item{creationDate}{beginning of current research,}
#' \item{objects}{number of saved objects,}
#' \item{dataSets}{number of loaded data sets,}
#' \item{plots}{number of saved plots,}
#' \item{tables}{number of saved tables,}
#' \item{models}{number of saved models,}
#' \item{trainedModels}{number of trained models,}
#' \item{instructions}{number of instructions}
#'
#' @export
#' @author Joanna Jablonska
#' @examples
#' \dontrun{
#'
#' # firt start hugo investigation
#' hugo_start_investigation("example")
#' # use some functions from hugo package
#' model <- lm(Sepal.Length ~ Petal.Length, data = iris)
#' hugo_memorise_model(model)
#' hugo_show_history()
#' # call hugo_show_statistics() to see hugo summaries of current investigation
#' hugo_show_statistics()
#' unlink("example", recursive = TRUE)
#' }
#'
hugo_show_statistics <- function(){

  .hugoEnv$history[length(.hugoEnv$history)+1]<-deparse(match.call())

  if (!file.exists(.hugoEnv$path)) {
    stop("Call hugo_start_investigation() for starting the new investigation.")
  }

  hugo_statistics <- list("creationDate" = numeric(1), "objects" = numeric(1), "dataSets" = numeric(1), "plots"= numeric(1),  "tables"= numeric(1),
                          "models"= numeric(1), trainedModels = numeric(1), "instructions"= numeric(1))

  time <- file.info(.hugoEnv$path)$mtime
  hugo_statistics$creationDate <- gsub( " .*$", "", time)

  hugo_statistics$objects <- if(dir.exists(paste0(.hugoEnv$path, "/memory/"))){
    length(list.files(paste0(.hugoEnv$path, "/memory/")))}

  hugo_statistics$dataSets <- if(dir.exists(paste0(.hugoEnv$path, "/data/"))){
    length(list.files(paste0(.hugoEnv$path,"/data/")))}

  if (dir.exists(paste0(.hugoEnv$path, "/gallery/"))) {
    png.files <- gsub("\\.png.*$", "", list.files(paste0(.hugoEnv$path, "/gallery/"), "*\\.png$"))
    pdf.files <- gsub("\\.pdf.*$", "", list.files(paste0(.hugoEnv$path, "/gallery/"), "*\\.pdf$"))
    rda.files <-  gsub("\\.rda.*$", "", list.files(paste0(.hugoEnv$path, "/gallery/"), "*\\.rda$"))
    md.files <- gsub("\\.md.*$", "", list.files(paste0(.hugoEnv$path, "/gallery/"), "*\\.md$"))
    docx.files <- gsub("\\.docx.*$", "", list.files(paste0(.hugoEnv$path, "/gallery/"), "*\\.docx$"))

    hugo_statistics$plots <- length(intersect(intersect(png.files, pdf.files), rda.files))
    hugo_statistics$tables <- length(docx.files)
    hugo_statistics$models <- length(intersect(intersect(md.files, rda.files), rda.files)) - length(docx.files)
  }

  if (dir.exists(paste0(.hugoEnv$path, "/models/"))) {
    hugo_statistics$trainedModels <- length(list.files(paste0(.hugoEnv$path, "/models/")))}

  hugo_statistics$instructions <- length(.hugoEnv$history)

  cat("Investigation:", .hugoEnv$path, "\n\n")
  print(hugo_statistics)
}
