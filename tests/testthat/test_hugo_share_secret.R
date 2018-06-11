context("Check hugo_share_secret and hugo_get_secret functions")

test_that("object is encrypted and decrypted", {
  encrypted_object <- encrypt_object(iris, "pass")
  decrypted_object <- decrypt_object(encrypted_object, "pass")
  expect_equal(iris, decrypted_object)
})

test_that("object is not decrypted with wrong password", {
  encrypted_object <- encrypt_object(iris, "pass")
  expect_error(decrypt_object(encrypted_object, "wrong_pass"))
})

hugo_share_object_success_mock <- function(object) {
  decrypted_iris <- decrypt_object(object, "pass")
  expect_equal(decrypted_iris, iris)
  return("hugo_get_object('user', 'repo', 'object_123')")
}

hugo_share_object_fail_mock <- function(object) {
  decrypted_iris <- decrypt_object(object, "pass")
  expect_equal(decrypted_iris, iris)
  return(NULL)
}

test_that("object is encrypted and shared", {
  with_mock(hugo_share_object = hugo_share_object_success_mock,
            {
              get_string <- hugo_share_secret(iris, "pass")
              expect_equal(get_string,
                           "hugo_get_secret('user', 'repo', 'object_123', 'pass')")
            })
})

test_that("should return NULL when there was error", {
  with_mock(hugo_share_object = hugo_share_object_fail_mock,
            {
              get_string <- hugo_share_secret(iris, "pass")
              expect_null(get_string)
            })
})

hugo_get_object_success_mock <- function(user, repo_name, object_name) {
  expect_equal(user, "user")
  expect_equal(repo_name, "repo")
  expect_equal(object_name, "iris_secret")
  return(encrypt_object(iris, "pass"))
}

hugo_get_object_fail_mock <- function(user, repo_name, object_name) {
  expect_equal(user, "user")
  expect_equal(repo_name, "repo")
  expect_equal(object_name, "iris_secret")
  return(NULL)
}

test_that("object is fetched and decrypted", {
  with_mock(hugo_get_object = hugo_get_object_success_mock,
            {
              decrypted_object <- hugo_get_secret("user", "repo", "iris_secret", "pass")
              expect_equal(decrypted_object, iris)
            })
})

test_that("get return NULL when there was an error during fetch", {
  with_mock(hugo_get_object = hugo_get_object_fail_mock,
            {
              object <- hugo_get_secret("user", "repo", "iris_secret", "pass")
              expect_null(object)
            })
})



requireNamespace_mock <- function(package, quietly) {
  return(FALSE)
}

test_that("function fails when there is no \"sodium\" package", {
  with_mock(requireNamespace = requireNamespace_mock,
    expect_error(hugo_share_secret(iris, "pass"))
  )
})

test_that("function fails when there is no \"sodium\" package", {
  with_mock(requireNamespace = requireNamespace_mock,
            expect_error(hugo_get_secret('user', 'repo', 'object_123', "pass"))
  )
})
