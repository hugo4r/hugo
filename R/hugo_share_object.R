#' Share any object using github
#'
#' @param object object to share
#' @param passphrase object will be encrypted using provided passphrase
#'
#' @export
#' @author Mateusz Krubiński, Kamil Romaszko
#' @examples
#' # TODO add some examples
#' hugo_share_object(iris)
hugo_share_object <- function(object, passphrase = NULL) {
  if (!is.null(passphrase)) {
    object <- encrypt_object(object, passphrase)
  }
  object
  # TODO implement share on github
}

#' @keywords internal
encrypt_object <- function(object, passphrase) {
  hashed_passphrase <- sodium::hash(charToRaw(passphrase))
  serialized_object <- serialize(object, NULL)
  encrypted_object <-
    sodium::data_encrypt(serialized_object, hashed_passphrase)

  return(encrypted_object)
}

#' @keywords internal
decrypt_object <- function(object, passphrase) {
  hashed_passphrase <- sodium::hash(charToRaw(passphrase))
  decrypted_object <-
    sodium::data_decrypt(object, hashed_passphrase)
  unserialized_object <- unserialize(decrypted_object)

  return(unserialized_object)
}
