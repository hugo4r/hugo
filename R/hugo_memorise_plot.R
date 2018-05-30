#' Saving plot as .rda, pdf and png
#'
#' @param plot
#' @param name
#'
#' @export
#' @author Joanna Golawska golawskaj
#' @examples hugo_memorise_plot()
#' 
#'
hugo_memorise_plot <- function(plot, name){
  return(0)
}



# funkcja do zapamiętywania wykresów w formacie rda oraz jego miniatura jako pdf i png
# obiekty zapamiętywane będę w katalogu badania w podkatalogu galery (jeśli nie
# istnieje taki katalog, to zostanie on utworzony)

# jako argumenty przyjmowane będą:
#   plot - w postaci funkcji plot podanej jako plot(), lub już zapamiętanej
#           jako recordedPlot jako obiekt, dozwolone są również wykresy stworzone
#           za pomocą ggplota (być może również przy wykorzystaniu innych pakietów)
#   name - pod jaką nazwą ma zostać zapisany obiekt, jesli uzytkownik nie poda nazwy
#           hugo zapyta się, czy użytkownik chce nadać konkretną, jeśli nie,
#           będą nadawane kolejne nazwy: plot1, plot2,...
#   
