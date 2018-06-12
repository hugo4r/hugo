#' Reads data to R
#'
#' Function unifies most common reading data functions. It guesses the file extenstion and fits best function to load.
#' Usually knowing types of parameters as: separator, decimal, header is not necessary, but function allows to put them by hand.
#' Supported extensions: "txt", "csv", "xls", "xlsx", "tsv", "rda", "rdata", "json".
#'
#'
#' @param path the name of the path which the data are to be read from. Each row of the table appears as one line of the file. If it does not contain an absolute path, the path name is relative to the current working directory, \code{getwd()}. path can also be a complete URL.
#'
#' @param file_extension the type of file which is loaded. Usually don't needed, because Hugo guesses it.
#'
#' @param header a logical value indicating whether the file contains the names of the variables as its first line. If missing, the value is determined from the file format: header is set to TRUE if and only if the first row contains one fewer field than the number of columns.
#'
#' @param separator the field separator character. Values on each line of the file are separated by this character. If sep = "" the separator is 'white space', that is one or more spaces, tabs, newlines or carriage returns.
#'
#' @param decimal the character used in the file for decimal points.
#'
#' @param file_name_to_save the name of the file where the data will be saved.
#'
#' @return Returns data.frame, tibble or a list.
#'
#' @export
#'
#' @author Dariusz Komosinski
#'
#' @examples
#' \dontrun{
#' ### simple loading most common types of extensions
#' #loading csv
#' path <- "http://insight.dev.schoolwires.com/HelpAssets/C2Assets/C2Files/C2SectionRobotSample.csv"
#' data <- hugo_read_data(path)
#' head(data)
#'
#' #loading rda
#' path <- system.file("extdata", "example.rda", package = "hugo")
#' data <- hugo_read_data(path)
#' head(data)
#'
#' #loading json
#' path <- "https://raw.githubusercontent.com/corysimmons/colors.json/master/colors.json"
#' data <- hugo_read_data(path)
#' head(data)
#'
#'
#' ### specifying our own parameters
#' #loading csv
#' path <- "http://insight.dev.schoolwires.com/HelpAssets/C2Assets/C2Files/C2SectionRobotSample.csv"
#' data <- hugo_read_data(path, separator = ",", decimal = ".", header = TRUE)
#' head(data)
#'
#'
#' ### interaction with user
#' # loading file without extension
#' path <- system.file("extdata", "no_extension", package = "hugo")
#' # input "txt"
#' data <- hugo_read_data(path)
#' head(data)
#'
#' # providing your own parameters
#' path <- system.file("extdata", "example2.txt", package = "hugo")
#' data <- hugo_read_data(path, decimal = ".")
#' # an error occured, but you have an information to put ALL parameters
#' data <- hugo_read_data(path, header = TRUE, separator = ",", decimal = ".")
#' head(data)
#'
#'
#' ### more examples
#' # for more examples please put an extension in <extension> below
#' # and try other avaliable sample files attached to package
#' # path <- system.file("extdata", "example.<extension>", package = "hugo")
#' # data <- hugo_read_data(path)
#' # head(data)
#'
#' }

hugo_read_data <- function(path, file_extension=NA, header=NA, separator=NA, decimal=NA, file_name_to_save=NULL){

  .hugoEnv$history[length(.hugoEnv$history)+1]<-deparse(match.call())

  # checks the correctness of the path
  if (is.null(path) ) {
    stop("Path was not provided!")
  }
  if(!file.exists(path) && !RCurl::url.exists(path)) {
    stop("File does not exist.")
  }

  # checks the extension of the file
  if(is.na(file_extension)){
    file_extension <- stringr::str_match(path, stringr::regex(".*(?<=\\.)(.*)"))[2]
  }
  if(is.na(file_extension) || file_extension=="" || is.null(file_extension)) {
    file_extension <- tolower(readline(cat("Please input file extension. > \n")))
    if(file_extension==""){
      cat("\nNo file extension provided. Set to default: txt")
      file_extension <- "txt"
    }
  }

  #formats <- c("txt", "csv", "xls", "xlsx", "tsv", "rda", "rdata", "json")
  formats <- c("txt", "csv", "xlsx", "tsv", "rda", "rdata", "json")
  if(!(file_extension %in% formats)){
    stop("File extension is not supported.")
  }

  # allows to provide own parameters
  if(all(!is.na(c(separator, decimal, header)))){
    read_data <- utils::read.table(path, header = header, sep = separator, dec = decimal)
  }
  if(sum(as.numeric(is.na(c(separator, decimal, header))))<=2 && sum(as.numeric(is.na(c(separator, decimal, header))))>=1){
    stop("Please provide all parameters: separator, decimal and header or none of them and trust Hugo.")
  }
  # when no parameters provided, hugo guesses them
  if(all(is.na(c(separator, decimal, header)))){
    # chooses function to load which fits the extension
    if(file_extension=="json"){
      read_data <- rjson::fromJSON(file=path)
    }
    if(file_extension=="rda" || file_extension=="Rdata"){
      env <- new.env()
      loaded_name <- load(path, env)
      read_data <- env[[loaded_name]]
    }
    if(file_extension=="tsv"){
      read_data <- readr::read_tsv(file = path)
    }
    if(file_extension=="csv"){
      read_data <- readr::read_csv(file=path)
      if(ncol(read_data)<=1){
        read_data <- readr::read_csv2(file=path)
        if(ncol(read_data)<=1){
          utils::head(read_data)
          stop("File haven't been loaded correctly. Please provide your all own parameters.")
        }
      }
    }
    if(file_extension=="txt"){
      read_data <- readr::read_table(file=path)
      if(ncol(read_data)<=1){
        read_data <- readr::read_table2(file=path)
        if(ncol(read_data)<=1){
          utils::head(read_data)
          stop("File haven't been loaded correctly. Please provide your all own parameters.")
        }
      }
    }
    if(file_extension=="xlsx"){
      read_data <- openxlsx::read.xlsx(path)
    }
    # if(file_extension=="xls" || file_extension=="xlsx"){
    #   read_data <- xlsx::read.xlsx(file=path, sheetIndex = 1)
    #   if(ncol(read_data)<=1){
    #     read_data <- xlsx::read.xlsx2(file=path, sheetIndex = 1)
    #     if(ncol(read_data)<=1){
    #       utils::head(read_data)
    #       stop("File haven't been loaded correctly. Please provide your all own parameters.")
    #     }
    #   }
    # }
  }


  # saves loaded data
  #path_to_save_data <- paste(getwd(), '/data/', sep = '')
  path_to_save_data <- paste(.hugoEnv$path, '/data/', sep = '')
  if(!dir.exists(path_to_save_data)) {
    dir.create(path_to_save_data)
  }

  if(is.null(file_name_to_save)){
    file_name_to_save <- "read_data.rda"
  }
  save(read_data, file=paste0(path_to_save_data, file_name_to_save))

  nr_of_columns <- ncol(read_data)
  nr_of_rows <- nrow(read_data)

  # prints a message
  cat(paste0("\nI've read a data with ", nr_of_rows, " rows and ", nr_of_columns, " columns."))
  cat(paste0("\nCopy of it is stored in ", path_to_save_data, file_name_to_save, "."))

  return(read_data)
}


