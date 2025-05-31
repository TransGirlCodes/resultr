test_that("inspect_fail works", {
  expect_no_message(Success(5) |> inspect_fail(~ message("Value was fail")) -> s_out)
  expect_message(Failure("Epic Fail") |> inspect_fail(~ message("Value was fail")) -> f_out)
  expect_equal(s_out, Success(5))
  expect_equal(f_out, Failure("Epic Fail"))
})
