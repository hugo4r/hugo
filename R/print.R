#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
print.hugo <- function(x, ...) {
  cat(random_greeting(), "\n")
}

random_greeting <- function() {
  sample(list_of_greetings, 1)
}
