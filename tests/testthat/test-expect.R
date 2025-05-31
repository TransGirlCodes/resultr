test_that("expect works", {
  expect_no_error(Success(5) |> expect("Panic!!!") -> s_out)
  expect_error(Failure("Epic Fail") |> expect("Panic!!!", class = "resultr::expect_panic") -> f_out)
  expect_equal(s_out, 5)
})

test_that("expect_fail works", {
  expect_error(Success(5) |> expect_fail("Panic!!!"), class = "resultr::expect_fail_panic")
  expect_no_error(Failure("Epic Fail") |> expect_fail("Panic!!!") -> f_out)
  expect_equal(f_out, "Epic Fail")
})
