#' Create report with summary of variables in dataset.
#'
#' Creates  the report 'html' or 'pdf' in the directory chosen at the beginning of the current
#' Hugo Investigation in the subfolder 'gallery' in .rda file and a summary in .md file.
#' This reports summarizes the contents of a dataset
#' and flags potential problems.
#' To generate report is used function \code{dataMaid::makeDataReport}.
#'
#' This function is interactive with user. With first usage user has to type parameters, in next usage user
#' can choose previous settings or type new. User can change filename and title of report.
#' User can change output format (\code{output}), decide to overwrite output (\code{replace_output}),
#'   decide whether numeric and integer variables with
#' less than 5 unique values are treated as factor variables (\code{smart_factor}),
#' type max decimals (\code{max_decimals}) and decide to automatically open report (\code{open_result}).
#'
#' @param data R dataset which you wish to summary. This dataset should be of class \code{data.frame},
#' \code{tibble} or \code{matrix}.
#' @param overwrite_params logical. If \code{TRUE} (the default) parameters used to generate report will be saved.
#'
#' @return The function does not return anything. Its side effect (the production
#' of a data report) is the reason for running the function.
#' @example
#' \dontrun{
#' hugo_summarise_data(iris)
#' }
#' @importFrom dataMaid makeDataReport
#' @export
#'
hugo_summarise_data <- function(data,
                                overwrite_params = TRUE
){

  .hugoEnv$history[length(.hugoEnv$history)+1]<-deparse(match.call())

  stopifnot(class(data) %in% c("data.frame", "tibble", "matrix"))
  stopifnot(is.logical(overwrite_params))


  if (!requireNamespace("dataMaid", quietly = TRUE)) {
    stop("Package \"dataMaid\" needed for this function to work.
         Install it first.",
         call. = FALSE)
  }


  data_name <- deparse(substitute(data))
  if (substr(data_name, 1, 11) == "data.frame(") {
    data_name <- "unnamed_data"
  }

  default_filename <-  paste0( "hugo_summary_", data_name)

  path <- normalizePath(paste0("./",.hugoEnv$path, "/gallery"))
    if (!dir.exists(path)) {
    dir.create(path)
    }



  columns_name <- colnames(data)

  cat(' Hugo\'s trying to prepare report for you. \n')

  if(!getOption("hugo.know_summary_parameters"  )){

    default_parameters <- get_parameters_from_user(columns_name)
    filename <- filename_input(default_filename, path)
    report_title <- title_input(data_name)
  }
   else{
     default_parameters <- .hugoEnv$summarise_data_default_params

     hugo_menu <- menu_prev_settings(default_parameters)

     if(is.null(hugo_menu)) {return()}
     if(!hugo_menu) {

       default_parameters <- get_parameters_from_user(columns_name)
     }

      filename <- filename_input(default_filename, path = path)
      report_title <- title_input(data_name)


   }





  filepath <- paste0(path, "/",  filename, ".Rmd")
  parameters <- c(list(data = quote(data), reportTitle = report_title,
                       file = filepath , codebook = TRUE,
                       quiet = "silent"), default_parameters )



  results <- tryCatch({suppressWarnings(do.call('makeDataReport',
                                                args = parameters
  ))},
  warning = function(w) {
    warning("While generating a report: ", w)
  }, error = function(e) {
    e$message <- paste0("While generating a report: ", e)
    stop(e)
  })
  add_path_to_history(paste0(.hugoEnv$path, "/gallery/",filename,".", parameters$output))

  if(overwrite_params){
    save_parameters(default_parameters)
    options(hugo.know_summary_parameters = T)
                      }


  cat("Success!")

  }




get_parameters_from_user <- function(columns_name){

  output <- get_output_from_user()
  if(is.null(output)) {
    output <- 'html'
    message("Default value will be used: html")
  }

  replace_output <- get_replace_from_user()
  if(is.null(replace_output)) {
    replace_output <- TRUE
    message("Default value will be used: TRUE")
  }
  open_output <- get_open_from_user()
  if(is.null(open_output)) {
    open_output <- FALSE
    message("Default value will be used: FALSE")
  }


  smart_factor <- get_smart_factor_from_user()
  if(is.null(smart_factor)) {
    smart_factor <- TRUE
    message("Default value will be used: TRUE")
  }



  cat("Please type max decimals: > ",file = getOption('hugo.connection_out'))
  max_decimals <- suppressWarnings(as.numeric(readLines(con = getOption('hugo.connection_in'), n = 1)))

  if((! max_decimals %% 1 == 0) | (is.na(max_decimals)) |( max_decimals > 10)||( max_decimals < 0)){
    message("Incorrect value. Default 2 will be used.")
    max_decimals <- TRUE
  }

  return(list(output = output,
              replace = replace_output,
              smartNum = smart_factor,
              maxDecimals = max_decimals,
              openResult = open_output))
}

menu_prev_settings <- function(default_parameters){
  hugo_menu <- switch(utils::menu(c('Use previous settings for summary.','Enter new settings.'),
                                  title = paste0("Found previous settings:  ",
                                                 paste(names(default_parameters),"=", unlist(as.character(default_parameters)), collapse = ", ")
                                  )),T,F)
  return(hugo_menu)
}

get_output_from_user <- function (){
  output <- switch(utils::menu(c('pdf','html'),
                               title = paste0("Please choose output format: > ")),'pdf','html')
  return(output)
}

get_replace_from_user <- function (){
  replace_output <- switch(utils::menu(c('TRUE','FALSE'),
                               title = paste0('Do you want to replace existing file: >')),TRUE,FALSE)
  return(replace_output)
}
get_open_from_user <- function (){
  open_output <- switch(utils::menu(c('TRUE','FALSE'),
                                       title = paste0('Do you want to open output file: > ')),TRUE,FALSE)
  return(open_output)
}

get_smart_factor_from_user <- function (){
  smart_factor <- switch(utils::menu(c('TRUE','FALSE'),
                                       title = paste0("Do you want to numeric colums with less than 5 unique values are treated as factor variables?: > ")),TRUE,FALSE)
  return(smart_factor)
}



filename_input <- function(default_filename, path){

  cat(paste0("To use \"", default_filename, "\" name type NULL or type your name of output (without path): > "), file = getOption('hugo.connection_out'))
  filename <- readLines(con = getOption('hugo.connection_in'), n = 1)
  if(! filename %in% c("NULL", "")){

    filename <-  basename(tools::file_path_sans_ext(filename))

  }
  else{
    message("Default filename will be used")
    filename <-  default_filename
  }
  return(filename)
}

title_input <- function(data_name){

  cat(paste0("To use \"", data_name, "\" as title type NULL or type your report title: > "),file = getOption('hugo.connection_out'))
  report_title <- readLines(con = getOption('hugo.connection_in'), n = 1)
  if(report_title %in% c("NULL", "")){
    message("Default title will be used")
    report_title <- data_name
  }

  return(report_title)
}




save_parameters <- function(parameters){


  .hugoEnv$summarise_data_default_params <- parameters
}

