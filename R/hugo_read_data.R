#' Reads data to R in many ways
#'
#' @param path name of a directory where is file to read
#'
#' @export
#' @author Dariusz Komosinski
#' @examples
#' hugo_read_data()
#'
hugo_read_data <- function(path){
  return(0)
}

#pomysl
# 1. najpierw bierze rozszerzenie pliku po kropce
# 2. w zaleznosci od tego co to korzysta z funkcji json, rda itd
# 3. jesli to jest textowy to uzywa tekstowego ALE
# 3.1 wywoluje z domyslnymi i patrzy czy est wiecej niz 1 kolumna
# jesli nie to zmienia parametr sep
# jesli nadal zle to zlicza kropki przecinki itd i patrzy ktorych najwiecej
# kombinuje miedzy sep a delimeter tak zeby dzialalo
# jesli nadal nie to wyswietla head i pyta uzytkownika co ma zrobic
# 4. zapisuje wczytane w formacie .rda
