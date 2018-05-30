#' Share any object using github
#'
#' @param object object to share
#'
#' @export
#' @author Mateusz Krubiński
#' @examples
#' # TODO add some examples
#' hugo_share_object(iris)
hugo_share_object <- function(object) {
  # TODO implement share on github
}


# Korzysta z bilbioteki 'git2r'.
# Przy pierwszym wywołaniu prosi użytkownika o podanie user/password
# oraz o wybór, czy obiekt umieszcza w zdalnym repozytorium,
# czy korzysta z lokalnej kopii do dodania obiektu.
# Przy kolejnych, pyta, czy korzystać z wczytanych parametrów, czy podać nowe.
# Zakładam, że korzystamy z repozytorium publicznego.
# Pobranie obiektu polega na odpowiednio eleganckim (opakowanym w funckję, np hugo::hugo_load(object) przekazaniu parametrów koniecznych do wczytaniu pliku.
