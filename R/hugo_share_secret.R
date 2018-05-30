#' Encrypt and share any object
#'
#' @param object object to share
#' @param passphrase object will be encrypted using provided passphrase
#'
#' @export
#' @author Kamil Romaszko
#' @examples
#' hugo_share_secret(iris, "secret")
hugo_share_secret <- function(object, passphrase) {

  if (!requireNamespace("sodium", quietly = TRUE)) {
    stop("Package \"sodium\" needed for this function to work.
          Install it if you really want ecryption or use hugo_share_object.",
         call. = FALSE)
  }
  object <- encrypt_object(object, passphrase)
  # object
  # TODO implement share on github
}

encrypt_object <- function(object, passphrase) {
  hashed_passphrase <- sodium::hash(charToRaw(passphrase))
  serialized_object <- serialize(object, NULL)
  encrypted_object <-
    sodium::data_encrypt(serialized_object, hashed_passphrase)

  return(encrypted_object)
}

decrypt_object <- function(object, passphrase) {
  hashed_passphrase <- sodium::hash(charToRaw(passphrase))
  decrypted_object <-
    sodium::data_decrypt(object, hashed_passphrase)
  unserialized_object <- unserialize(decrypted_object)

  return(unserialized_object)
}
