test_that("is_success works", {
  expect_true(is_success(Success(5)))
  expect_false(is_success(Failure("Epic Fail")))
})
