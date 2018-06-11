#' Encrypt and share any object
#'
#' Encrypt and share R objects on github
#'
#' @param object object to share
#' @param passphrase object will be encrypted using provided passphrase
#'
#' @param user Github user name, to construct proper url to repository.
#' @param repo_name Name of github repository, also used to construct url.
#' @param object_name Name of the object you want to download.
#'
#' @describeIn  hugo_share_secret Shares given object via github, prompts user for input.
#'
#'    Returns: in case of success prompts user with message, and returns string, which is
#'    direct call to hugo_get_secret. In case of failure, prompts user with appropriate
#'    message and returns NULL.
#'
#' @return Please check Functions section.
#' @export
#' @author Kamil Romaszko
#' @examples
#' \dontrun{
#' hugo_share_secret(iris, "secret")
#' }
hugo_share_secret <- function(object, passphrase) {
  if (!requireNamespace("sodium", quietly = TRUE)) {
    stop(
      "Package \"sodium\" is needed for this function to work.
      Install it if you want encryption or use hugo_share_object instead."
    )
  }

  object <- encrypt_object(object, passphrase)
  get_string <- hugo_share_object(object)
  if(is.null(get_string)) {
    return(get_string)
  }

  get_string <- get_string_with_passphrase(get_string, passphrase)

  return(get_string)
}


#' @describeIn  hugo_share_secret Downloads encrypted object from github.
#'    Returns: in case of success prompts user with message, and returns object.
#'    It us up to user how to handle it - wheter to assign it to variable,
#'    or use as parameter. In case of failure, prompts user with appropriate
#'    message and returns NULL.
#'
#' @export
hugo_get_secret <- function(user, repo_name, object_name, passphrase) {
  if (!requireNamespace("sodium", quietly = TRUE)) {
    stop(
      "Package \"sodium\" is needed for this function to work.
      Install it if you want download encrypted file."
    )
  }

  encrypted_object <- hugo_get_object(user, repo_name, object_name)
  if(is.null(encrypted_object)) {
    return(encrypted_object)
  }
  decrypted_object <- decrypt_object(encrypted_object, passphrase)

  return(decrypted_object)
}


encrypt_object <- function(object, passphrase) {
  hashed_passphrase <- sodium::hash(charToRaw(passphrase))
  serialized_object <- serialize(object, NULL)
  encrypted_object <- sodium::data_encrypt(serialized_object, hashed_passphrase)

  return(encrypted_object)
}


decrypt_object <- function(object, passphrase) {
  hashed_passphrase <- sodium::hash(charToRaw(passphrase))
  decrypted_object <- sodium::data_decrypt(object, hashed_passphrase)
  unserialized_object <- unserialize(decrypted_object)

  return(unserialized_object)
}


get_string_with_passphrase <- function(get_string, passphrase) {
  get_string <- sub('object', 'secret', get_string)
  string_len <- nchar(get_string)
  passphrase_to_add <- paste0(", '", passphrase, "'")
  get_string <- paste(substring(get_string,
                                c(1, string_len),
                                c(string_len - 1, string_len)),
                      collapse = passphrase_to_add)

  return(get_string)
}
