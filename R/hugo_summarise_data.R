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
#' @examples
#' \dontrun{
#' hugo_summarise_data(iris)
#' hugo_summarise_data(cars)
#' }
#' @importFrom dataMaid makeDataReport
#' @export
#'
hugo_summarise_data <- function(data,
                                overwrite_params = TRUE
){

  if (!file.exists(.hugoEnv$path)) {
    stop('Call hugo_start_investigation() for starting the new investigation.')
  }

  .hugoEnv$history[length(.hugoEnv$history)+1]<-deparse(match.call())

  if (! class(data) %in% c("data.frame", "tibble", "matrix")){
    stop("Incorrect data class. It\'s not a data.frame or tibble.")
  }
  stopifnot()
  if (! is.logical(overwrite_params)){
    stop("Incorrect argument type.  ")
  }


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


  cat(' Hugo\'s trying to prepare report for you. \n')

  names_parameters <- c("Output format ", "Replace output ", "Smart factor ", "Max Decimals ", "Open report ")

  if (!getOption("hugo.know_summary_parameters"  )){
      parameters_to_replace <- list(output = "html",
                                 replace = TRUE,
                                 smartNum = TRUE,
                                 maxDecimals = 2,
                                 openResult = TRUE)
      hugo_menu <- menu_first_settings (parameters_to_replace, names_parameters)


  }

  else {
      parameters_to_replace <- .hugoEnv$summarise_data_default_params
      hugo_menu <- menu_prev_settings(parameters_to_replace, names_parameters)

  }

  if (is.null(hugo_menu)) {return()}
  if (!hugo_menu) {

    used_parameters <- get_parameters_from_user()
  }

  else{

    used_parameters <- parameters_to_replace

  }


  filename <- filename_input(default_filename, path = path)
  report_title <- title_input(data_name)



  filepath <- paste0(path, "/",  filename, ".Rmd")
  parameters <- c(list(data = quote(data), reportTitle = report_title,
                       file = filepath , codebook = TRUE,
                       quiet = "silent"), used_parameters )



  results <- tryCatch({suppressWarnings(do.call('makeDataReport',
                                                args = parameters
  ))},
  warning = function(warning_communicate) {
    warning("From package \"dataMaid\" while creating a report: ", warning_communicate)
  }, error = function(error_communicate) {
    error_communicate$message <- paste0("From package \"dataMaid\" while creating a report: ", error_communicate)
    stop(error_communicate)
  })

  add_path_to_history(paste0(.hugoEnv$path, "/gallery/",filename,".", parameters$output))

  if (overwrite_params){
    save_parameters(used_parameters)
    options(hugo.know_summary_parameters = TRUE)
  }

  cat("Report ", filename, " saved in ", paste0(.hugoEnv$path, "/gallery/"), "\n")
  cat("Success!")

  }




get_parameters_from_user <- function(){

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
  if (is.null(open_output)) {
    open_output <- FALSE
    message("Default value will be used: FALSE")
  }
  if (open_output) {
    message("Close previous report!")
  }


  smart_factor <- get_smart_factor_from_user()
  if (is.null(smart_factor)) {
    smart_factor <- TRUE
    message("Default value will be used: TRUE")
  }



  cat("Please type max decimals: > ",file = getOption('hugo.connection_out'))
  max_decimals <- suppressWarnings(as.numeric(readLines(con = getOption('hugo.connection_in'), n = 1)))

  if  ((! max_decimals %% 1 == 0) | (is.na(max_decimals)) |( max_decimals > 10)||( max_decimals < 0)){
    message("Incorrect value. Default 2 will be used.")
    max_decimals <- 2
  }

  return(list(output = output,
              replace = replace_output,
              smartNum = smart_factor,
              maxDecimals = max_decimals,
              openResult = open_output))
}

hugo_choose_menu <- function(text_options, value_options, title){
  hugo_menu <- switch(utils::menu(text_options,
                                  title = title),
                      value_options[1], value_options[2])
  return(hugo_menu)
}
menu_first_settings <- function(default_parameters, names_parameters){
  hugo_menu <-  hugo_choose_menu(c('Use default settings for report.','Enter new settings.'),
                                 c(TRUE, FALSE),
                                 title = paste0("Default settings:  ",
                                                paste(names_parameters,"= ", unlist(as.character(default_parameters)), collapse = ", "))
  )
  return(hugo_menu)
}


menu_prev_settings <- function(default_parameters, names_parameters){
  hugo_menu <-  hugo_choose_menu(c('Use previous settings for report.','Enter new settings.'),
                                 c(TRUE, FALSE),
                                 title = paste0("Found previous settings:  ",
                                                paste(names_parameters,"= ", unlist(as.character(default_parameters)), collapse = ", "))
                                 )
  return(hugo_menu)
}

get_output_from_user <- function (){
  output <- hugo_choose_menu(c('pdf','html'),
                             c('pdf','html'),
                             'Please choose report format: > ')
     return(output)
}



get_replace_from_user <- function (){
  replace_output <- hugo_choose_menu(c('TRUE', 'FALSE'),
                                     c(TRUE, FALSE),
                                     'Do you want to replace existing report with the same name: >')
  return(replace_output)
}


get_open_from_user <- function (){
  open_output <- hugo_choose_menu(c('TRUE', 'FALSE'),
                                  c(TRUE, FALSE),
                                  'Do you want to open report after creating: > ')
  return(open_output)
}


get_smart_factor_from_user <- function (){
  smart_factor <- hugo_choose_menu(c('TRUE', 'FALSE'),
                                   c(TRUE, FALSE),
                                   "Do you want to numeric columns with less than 5 unique values are treated as factor variables?: > ")
    return(smart_factor)
}



filename_input <- function(default_filename, path){

  cat(paste0("To use \"", default_filename, "\" name type 0 or type your name of report (without path): > "), file = getOption('hugo.connection_out'))
  filename <- readLines(con = getOption('hugo.connection_in'), n = 1)
  if (! filename %in% c("NULL", "", "0", "null")){
    filename <-  basename(tools::file_path_sans_ext(filename))
  }
  else{
    message("Default filename will be used")
    filename <-  default_filename
  }
  return(filename)
}

title_input <- function(data_name){

  cat(paste0("To use \"", data_name, "\" as title type 0 or type your report title: > "),file = getOption('hugo.connection_out'))
  report_title <- readLines(con = getOption('hugo.connection_in'), n = 1)
  if (report_title %in% c("NULL", "", "0", "null")){
    message("Default title will be used")
    report_title <- data_name
  }

  return(report_title)
}




save_parameters <- function(parameters){
  .hugoEnv$summarise_data_default_params <- parameters
}

