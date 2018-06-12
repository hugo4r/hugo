#' Create report with variables summary in dataset
#'
#' Creates  the report 'html' or 'pdf' in the directory chosen at the beginning of the current
#' Hugo Investigation in the subfolder 'gallery' in .rda file and a summary in .md file.
#' Make a data overview report that summarizes the contents of a dataset
#' and flags potential problems. The potential problems are identified by
#' running a set of class-specific validation checks, so that different
#' checks are performed on different variables types.
#'
#' @param data The dataset to be summarised. This dataset should be of class \code{data.frame},
#' \code{tibble} or \code{matrix}. If it is of classs \code{matrix}, it will be converted to a
#' \code{data.frame}.
#' @param earlier_settings A logical. If \code{TRUE} (the default) and this function was used in investigation, saved parameters will be used.
#' Parameters given by user are overwrite by saved arguments.
#' @param output Output format. Options are \code{"pdf"}, \code{"word"} (.docx) and \code{"html"}. This parameters If \code{NULL} (the default),
#' the output format depends two sequential checks. First, whether a LaTeX installation is available,
#' in which case \code{pdf} output is chosen. Secondly, if no LaTeX installation
#' is found, then if the operating system is Windows, \code{word} output is used. Lastly, if neither of these
#' checks are positive, \code{html} output is used.
#' @param mode Vector of tasks to perform among the three categories "summarize", "visualize" and "check".
#' The default, \code{c("summarize", "visualize", "check")}, implies that all three steps are
#' performed. The steps selected in \code{mode} will be performed for each variable in
#' \code{data} and their results are presented in the second part of the outputtet data report.
#' The "summarize" step is responsible for creating the summary table,
#' the "visualize" step is responsible for creating the plot and the "check" step is responsible
#' for performing checks on the variable and printing the results if any problems are found.
#' @param report_title A text string. If supplied, this will be the printed title of the
#' report. If left unspecified, the title with the name of the supplied dataset.
#'
#' @return The function does not return anything. Its side effect (the production
#' of a data report) is the reason for running the function.
#' @importFrom dataMaid makeDataReport
#' @export
#'
#' @examples
hugo_summarise_data <- function(data, earlier_settings = TRUE,
                                output = "pdf",
                                replace = TRUE,
                                mode = "check",
                                report_title = NULL){

  # argument overwrite_parameters

  # zmiana nazwy outputu

  # jesli was_used == T i earlier_settings = True komunikat o nadpisaniu

  #checking whether is correct default_settings
  # pomysl: funkcja moze sprawdzac ile jest zmiennych i jesli jest ich duzo zapytac czy na pewno
  #chce podsumowac  wszystkie

  # sprawdzenie typow kolumn, jesli nie wszystkie sa obslugiwane to zapyta uzytkownika jak je zrzutowac

  add_to_history("hugo_summarise_data")


  if (!requireNamespace("dataMaid", quietly = TRUE)) {
    stop("Package \"dataMaid\" needed for this function to work.
         Install it first.",
         call. = FALSE)
  }

  cat('hugo: I\'m trying to prepare report for you. \n')

  stopifnot(is.logical(earlier_settings))

  if (earlier_settings){
    was_used <- check_history(data)
    print(was_used)
    if(!was_used){
      if(missing(earlier_settings)){
        answer <- readline("This is the first usage of this function.\n You choose to use default settings for generate report. Are you sure? y/n")
        if(! answer %in% c("y","n") ){
          cat("Incorrect value. We have to stop generate report.")
        }
        if( answer == "n") {
          stop(" We have to stop generate report.")

        }
        if( answer == "y") {
          cat(" We will generate report with default settings.")

        }

      }
    }

  }

  else was_used <- FALSE
  # path <- paste0(.hugoEnv$path, "/gallery")
  # if (!dir.exists(path)) {
  #   dir.create(path)
  # }





  parameters <- get_parameters(data, earlier_settings = earlier_settings, was_used,
                               output = output,
                               replace = replace,
                               report_title = report_title,
                               mode = mode)

  print(parameters)

  resluts <- tryCatch({do.call(eval(parse(text='dataMaid::makeDataReport')),args = parameters )},
                      warning = function(w) {
                        warning("While generating a report: ", w)
                      }, error = function(e) {
                        e$message <- paste0("While generating a report: ", e)
                        stop(e)
                      })
  # resluts <- tryCatch({dataMaid:::makeDataReport(data = parameters[[1]],
  #                                               output = parameters[[2]],
  #                                               reportTitle = parameters[[3]],
  #                                               mode = parameters[[4]])},
  #                     warning = function(w) {
  #                       warning("While generating a report: ", w)
  #                     }, error = function(e) {
  #                       e$message <- paste0("While generating a report: ", e)
  #                       stop(e)
  #                     })
  # zmiana bledow

  save_parametrs(parameters)
  #
  #return(parameters)
}


# this function check whether function was used in history
check_history <- function(data, function_name = "hugo_summarise_data"){

  index <- any(grepl(function_name, .hugoEnv$history[1:(length(.hugoEnv$history)-1)]))

  return(index)
}




get_parameters <- function(data, earlier_settings ,was_used = FALSE, output , replace, report_title , mode  ){
  if(was_used &  earlier_settings){
    #list_files <- list.files(".")
    if(exists("summarise_data_default_params", envir = .hugoEnv))
      parameters <- .hugoEnv$summarise_data_default_params
    else{
      cat("Default parameters are not found in directory. Default settings will be used.\n")
      parameters <- create_parameters_list(data = quote(data), output = output,
                                           replace = replace,
                                           report_title = report_title,
                                           mode = mode)
    }

  }
  else{
    parameters <- create_parameters_list(data = quote(data),
                                         output = output,
                                         replace = replace,
                                         report_title = report_title,
                                         mode = mode)
  }
  return(parameters)
}




create_parameters_list <- function(data, output = NULL, replace, report_title = NULL, mode){
  default_parameters<- list(data = quote(data),
                             output = output,
                              replace = replace,
                             reportTitle = report_title,
                             mode = mode)

  #additional_parameters <- list(...)
  # komunikat: parameters from ... will be overriden by default_parameters

  default_parameters
}


save_parametrs <- function(parameters){
  # if(exists("summarise_data_default_params", envir = .hugoEnv)) {
  #   assign('summarise_data_default_params',parameters, envir = .hugoEnv)
  # }
  # if(!exists("summarise_data_default_params", envir = .hugoEnv)) {
  #   assign('summarise_data_default_params',parameters, envir = .hugoEnv)
  # }

  .hugoEnv$summarise_data_default_params <- parameters
}
