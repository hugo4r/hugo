#' Dispays history of hugo investigation.
#'
#' Each usage of any hugo function adds an entry about it to a hidden variable in hugo environment 
#' which contains the whole history of your investigation.
#' hugo_show_history(), displays all the entries with additions of paths to files created by functions hugo_memorize*.
#' 
#' @param last the number of entries from the beginning of the history to display.
#' @param first the number of entries from the end of the history to display.
#' @param specyfic is a natural vector ponting out which particular enrties should be shown.
#' @param restart If true, the history of investigation will reset.
#' @note maximum one argument may not be NULL.
#' 
#' Providing variables which would result in showing more entries than the current history contains 
#' or showing entries with indexes not belonging to history index set, cuts displaying. See "examples".
#' 
#' @export
#' @author Maciej Kurek
#' @examples
#' \dontrun{
#' # Displaying lat 10 entries of the investigation history:
#' hugo_show_history(10)
#'
#' # Displaying first 10 entries:
#' hugo_show_history(first=10)
#' 
#' # Displaying some specyfic entries:
#' hugo_show_history(specyfic=c(1,4,7))
#' 
#' # Let's assume there are 10 entries in the history. 
#' # Following usage will display all of theese entries.
#' 
#' hugo_show_history(20), hugo_show_history(first=15)
#' 
#' # This usage will show only entries with indexes 1,3,7 :
#' 
#' hugo_show_history(specyfic=c(-2,1,3,7,12,15))
#' 
#'  # Reseting history :
#' hugo_show_history(reset = T)
#' }

hugo_show_history<-function(last=NULL,first=NULL,specyfic=NULL,restart=F){
  
  .hugoEnv$history[length(.hugoEnv$history)+1]<-deparse(match.call())
  
  if(restart){
    .hugoEnv$history<-"empty" 
    return(cat("History of hugoing has been reseted"))
  }
  checking(last=last,first=first,specyfic=specyfic,restart=restart)
  
  if(.hugoEnv$history[1]=="empty"){
    .hugoEnv$history<-.hugoEnv$history[-1]
  }
  
  n<-length(.hugoEnv$history)
  which_to_show<-1:n
  
  if(!is.null(last)){
    which_to_show<-(n-last+1):n
  }
  
  if(!is.null(first)){
    which_to_show<-1:first
  }
  
  if(!is.null(specyfic)){
    which_to_show<-specyfic
  }
  
  which_to_show<-which_to_show[which_to_show>=1 & which_to_show<=n]
  



  cat("This is your hugoing history: \n \n")
  
  for (i in which_to_show)
  {
    cat(paste0(i,") ",.hugoEnv$history[i]),"\n")
  }
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

checking<-function(last=NULL,first=NULL,specyfic=NULL,restart=F)
{
  if(is.null(last)+is.null(first)+is.null(specyfic)<2) 
    stop("Error: Maximum one argument may not be NULL")
  ### last & first check
  
  if(!is.logical(restart))
    stop("Error: argument'restart' must be a logical value")
  if(!length(restart)==1)
    stop("Error: argument 'restart' must be a single value")  
  
  if(!is.null(first)) last<-first
  
  if(!is.null(last)){
    if(!is.numeric(last))
      stop("Error: argument 'last' must be a natural number")
    if(!length(last)==1)
      stop("Error: argument 'last' must be a natural number")  
    if(!last==floor(last))
      stop("Error: argument 'last' must be a natural number")
    if(last<=0)
      stop("Error: argument 'last' must be a natural number")  
  }
  ### specyfic check
  if(!is.null(specyfic)){
    if(!is.vector(specyfic))
      stop("Error: argument 'specyfic' must be a vector")
    if(!is.numeric(specyfic))
      stop("Error: argument 'specyfic' must contain natural numbers")
    if(!sum(specyfic==floor(specyfic))==length(specyfic))
      stop("Error: argument 'specyfic' must contain natural numbers")
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
