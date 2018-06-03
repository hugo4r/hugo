context("Check hugo_share_secret() function")

test_that("object is encrypted and decrypted", {
  encrypted_object <- encrypt_object(iris, "pass")
  decrypted_object <- decrypt_object(encrypted_object, "pass")
  expect_equal(iris, decrypted_object)
})

test_that("object is not decrypted with wrong password", {
  encrypted_object <- encrypt_object(iris, "pass")
  expect_error(decrypt_object(encrypted_object, "wrong_pass"))
})

test_that("object is encrypted and shared", {
  # TODO test encrypted share when it's ready
  expect_silent(hugo_share_secret(iris, "pass"))
})

requireNamespace_mock <- function(package, quietly) {
  return(false)
}

test_that("encryption is not used when there is no \"sodium\" package", {
  with_mock(requireNamespace = requireNamespace_mock,
    expect_error(hugo_share_secret(iris, "pass"))
  )
})
