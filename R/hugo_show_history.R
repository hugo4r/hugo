#' Dispays history of hugo investigation.
#'
#' Each usage of any hugo function adds an entry about it to a global variable called hugo_history, and the very first usage creates it.
#' hugo_show_history(), displays all the entries with additions of paths to files created by functions hugo_memorize*.
#'
#' @export
#' @author Maciej Kurek
#' @importFrom utils savehistory

hugo_show_history<-function(){

  add_to_history("hugo_show_history")

  cat("This is your hugoing history: \n \n")
  for ( i in 1:length(.hugoEnv$history))
  {
    cat(paste0(i,") ",.hugoEnv$history[i]),"\n")
  }
}

add_to_history<-function(function_name){
  savehistory()
  hist<-readLines(".Rhistory")
  # hist<-paste(hist[-length(hist)], collapse = '')
  file.remove(".Rhistory")

  pat<-paste(function_name,"\\(",sep="")
  index<-which(grepl(pat,hist[]))

  text<-paste(hist[max(index):length(hist)],collapse = "")

  if(.hugoEnv$history[1]!="empty") .hugoEnv$history[length(.hugoEnv$history)+1]<-text
  if(.hugoEnv$history[1]=="empty") .hugoEnv$history[1]<-text
}



