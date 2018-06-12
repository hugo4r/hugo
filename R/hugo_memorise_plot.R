#' Save plot
#'
#' Saves a plot in the directory chosen at the beginning of the current
#' Hugo Investigation in the subfolder 'gallery' in .rda file with miniatures
#' in pdf and png, what makes plot easier to search
#'
#' @param plot plot to be saved, created with package graphics or ggplot2
#' @param name character string, name of plot, if NULL then plots will be saved as "plot" with next numbers
#' @details  When plot to save is created with more then one funkction from package
#' graphics, the whole block of code should be putted to the function in bracket '{...}'.
#'
#' @export
#' @author Joanna Golawska
#' @examples
#' \dontrun{
#' # saving plot created with package graphics with a given name
#' hugo_memorise_plot(plot(1:10), "nice_plot")
#'
#' # saving plot created with package graphics without argument name
#' hugo_memorise_plot(plot(1:10, las = 1))
#'
#' # if plot is created with more then one function from package graphics
#' hugo_memorise_plot({plot(1:10)
#'                      abline(a = 1, b = 1)})
#'
#' # creating and saving plot with ggplot2
#' df <- data.frame(gp = factor(rep(letters[1:3], each = 10)),y = rnorm(30))
#' require(ggplot2)
#' plot_gg <- ggplot(df, aes(gp, y)) + geom_point()
#'
#' hugo_memorise_plot(plot_gg, "plot_with_ggplot")
#' # also works:
#' hugo_memorise_plot(ggplot(df, aes(gp, y)) + geom_point())
#' }
hugo_memorise_plot <- function(plot = NA, name = NULL){

  .hugoEnv$history[length(.hugoEnv$history)+1]<-deparse(match.call())

  if (!is.null(plot) & !ggplot2::is.ggplot(plot)){
    stop("Object to save is not a plot", call. = FALSE)
  }
  
  loadNamespace("ggplot2")

  path <- paste0(.hugoEnv$path, "/gallery")

  if (dir.exists(path) == FALSE) {
    dir.create(path)
  }

  if (is.null(name)) {
    plot_list <- list.files(path = path, pattern = "(plot)")
    if(length(plot_list)==0){name <- "plot01"}else{
    number <- max(as.numeric(substr(plot_list, start = 5L, stop = 6L)))
    if(number < 10){
      name <- paste0("plot", "0", number + 1)
    }else{
    name <- paste0("plot", number + 1)
    }
  }}
  
  if(any(list.files(path) == paste0(name, ".rda"))) {
    stop("File with name already exist. Please give another name. List of used names: " , list.files(path = path, pattern = "(.rda)"))
  }
  
 if(ggplot2::is.ggplot(plot)){
    save(plot, file = paste0(path, "/", name, ".rda"))
    ggplot2::ggsave(filename = paste0(path, "/", name, ".pdf"), plot = plot, device = "pdf")
    ggplot2::ggsave(filename = paste0(path, "/", name, ".png"), plot = plot, device = "png")
  }else{
    plot
    recorded_plot <- grDevices::recordPlot()
    grDevices::dev.off()

    save(recorded_plot, file = paste0(path, "/", name, ".rda"))

    grDevices::pdf(paste0(path, "/", name, ".pdf"))
    grDevices::replayPlot(recorded_plot)
    grDevices::dev.off()

    grDevices::png(paste0(path, "/", name, ".png"))
    grDevices::replayPlot(recorded_plot)
    grDevices::dev.off()
  }

  add_path_to_history(paste0(path, "/", name, ".pdf"))
  add_path_to_history(paste0(path, "/", name, ".png"))
  add_path_to_history(paste0(path, "/", name, ".rda"))
  
  cat("Your plot ", name, " has been saved in ", path, "\n")

}




