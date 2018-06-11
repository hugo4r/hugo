' Dispays history of hugo investigation.
#'
#' Each usage of any hugo function adds an entry about it to a hidden variable in hugo environment 
#' which contains the whole history of your investigation.
#' hugo_show_history(), displays all the entries with additions of paths to files created by functions hugo_memorize*.
#' 
#' @export
#' @author Maciej Kurek

hugo_show_history<-function(){
  
  add_to_history("hugo_show_history")
  
  cat("This is your hugoing history: \n \n")
  for ( i in 1:length(.hugoEnv$history))
  {
    cat(paste0(i,") ",.hugoEnv$history[i]),"\n")
  }
}

add_to_history<-function(function_name){
  
  utils::savehistory("histor.txt")
  hist<-readLines("histor.txt")
  file.remove("histor.txt")
  
  pat<-paste(function_name,"\\(",sep="")
  index<-which(grepl(pat,hist[]))
  
  text<-paste(hist[max(index):length(hist)],collapse = "")
  
  if(.hugoEnv$history[1]!="empty") .hugoEnv$history[length(.hugoEnv$history)+1]<-text
  if(.hugoEnv$history[1]=="empty") .hugoEnv$history[1]<-text
}

add_path_to_history<-function(path){
  path<-paste0(getwd(),"/",path)
  n<-length(.hugoEnv$history)

  if(grepl("\n Saved files: \n", .hugoEnv$history[n])){
    .hugoEnv$history[n]<-paste0(.hugoEnv$history[n],"\n",path)
  }
  
  if(!grepl("\n Saved files: \n", .hugoEnv$history[n])){
    .hugoEnv$history[n]<-paste0(.hugoEnv$history[n],"\n Saved files: \n",path)
  } 
}
