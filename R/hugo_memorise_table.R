#' Saves a table
#'
#' Saves a table in the directory chosen at the beginning of the current
#' Hugo Investigation in the subfolder 'gallery' as .rda file,  markdown table in .md file,
#' .xlsx, and .docx.
#'
#' @param model a table to be saved
#' @param name a name of files
#'
#' @export
#' @author Maria Piliszek
#' @examples
#' \dontrun{ a <- 1:3
#' tab <- table(a, sample(a))
#' hugo_memorise_table(tab, name = "table")
#' # Or save as "tab":
#' hugo_memorise_table(tab)}


hugo_memorise_table <- function(table = NULL, name = substitute(table)){
  
  # add_to_history("hugo_memorise_table")
  
  if (!file.exists(.hugoEnv$path)) {
    stop('Call hugo_start_investigation() for starting the new investigation.')
  }
  
  if (is.null(table)) {
    stop("There is no table to be saved.", call.=FALSE)
  }
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("You have to first install library 'openxlsx'. ", call. = FALSE)
  }
  
  if (!requireNamespace("ReporteRs", quietly = TRUE)) {
    stop("You have to first install library 'ReporteRs'. ", call. = FALSE)
  }
  
  path <- paste0(.hugoEnv$path, "/gallery")
  if (!dir.exists(path)) {
    dir.create(path)
  } 
  
  tryCatch({
    save(table, file=paste0(path, "/", name,".rda"))
  }, warning = function(w) {
    warning("While saving a table: ", w)
  }, error = function(e) {
    e$message <- paste0("While saving a table: ", e)
    stop(e)
  })
  
  tryCatch({
  doc <- ReporteRs::docx()
  doc <- ReporteRs::addFlexTable(doc, FlexTable(table))
  ReporteRs::writeDoc(doc, file = paste0(path, "/", name, ".docx"))
  },warning = function(w) {
    warning("While saving a table in .docx: ", w)
  }, error = function(e) {
    e$message <- paste0("While saving a table in .docx: ", e)
    stop(e)
  })
  
  tryCatch({
  openxlsx::write.xlsx(table, paste0(path, "/", name, ".xlsx"))
  },warning = function(w) {
    warning("While saving a table in .xlsx: ", w)
  }, error = function(e) {
    e$message <- paste0("While saving a table in .xlsx: ", e)
    stop(e)
  })
  
  tryCatch({
    rfile <- file(paste0(path, "/", name, ".md"), open="wt")
    sink(rfile, type="output")
    cat("Table in MarkDown:", "\n")
    knitr::kable(table)
  }, warning = function(w) {
    warning("From package \"knitr\" while creating a table: ", w)
  }, error = function(e) {
    e$message <- paste0("From package \"knitr\" while creating a table: ", e)
    stop(e)
  }, finally = {
    sink(type="output")
    close(rfile)
  })
  
}
