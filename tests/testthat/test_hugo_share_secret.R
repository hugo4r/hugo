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
