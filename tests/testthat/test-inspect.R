test_that("inspect works", {
  expect_message(Success(5) |> inspect(~ message("Value was success")) -> s_out)
  expect_no_message(Failure("Epic Fail") |> inspect(~ message("Value was success")) -> f_out)
  expect_equal(s_out, Success(5))
  expect_equal(f_out, Failure("Epic Fail"))
})
