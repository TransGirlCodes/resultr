test_that("unwrap works", {
  expect_no_error(Success(5) |> unwrap() -> s_out)
  expect_error(Failure("Epic Fail") |> unwrap() -> f_out, class = "resultr::unwrap_panic")
  expect_equal(s_out, 5)
})

test_that("unwrap_or_default works", {
  expect_equal(Success(5) |> unwrap_or_default(10), 5)
  expect_equal(Failure("Epic Fail") |> unwrap_or_default(10), 10)
})

test_that("unwrap_fail works", {
  expect_error(Success(5) |> unwrap_fail(), class = "resultr::unwrap_fail_panic")
  expect_no_error(Failure("Epic Fail") |> unwrap_fail() -> fail_val)
  expect_equal(fail_val, "Epic Fail")
})
