test_that("is_failure works", {
  expect_false(is_failure(Success(5)))
  expect_true(is_failure(Failure("Epic Fail")))
})
