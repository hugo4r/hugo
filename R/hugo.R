#' General dispatcher for hugo
#'
#' @param ... parameters for the dispatcher
#'
#' @return a \code{hugo} object
#' @export
#'
#' @examples
hugo <- function(...) {
   x <- list()


   class(x) = "hugo"
   x
}
