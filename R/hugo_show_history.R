#' Dispays history of hugo investigation.
#'
#' Each usage of any hugo function adds an entry about it to a global variable called hugo_history, and the very first usage creates it.
#' hugo_show_history(), displays all the entries with additions of paths to files created by functions hugo_memorize*.
#' 
#' @export
#' @author Maciej Kurek

hugo_show_history<-function(){
  
  add_to_history("hugo_show_history")
  
  cat("This is your hugoing history: \n \n")
  for ( i in 1:length(hugo_history))
  {
    cat(paste0(i,") ",hugo_history[i]),"\n")
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
  
  if(exists("hugo_history")) hugo_history[length(hugo_history)+1]<<-text
  if(!exists("hugo_history")) hugo_history<<-text
}


