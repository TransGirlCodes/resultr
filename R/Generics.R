
#' @export
inspect <- S7::new_generic("inspect", "x", function(x, fn, ...) {
  S7::S7_dispatch()
})

#' @export
inspect_fail <- S7::new_generic("inspect_fail", "x", function(x, fn, ...) {
  S7::S7_dispatch()
})


# Unwrapping values -------------------------------------------------------

#' Get the wrapped value from a `Result` or `Option` object
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' If the provided object is a `Failure` (in the case of a `Result` input) or
#' `Nothing` (in the case of an `Option` input), then the function will 'panic'
#' and throw an error of class `expect_panic`, with the message body set to
#' `msg`.
#'
#' @param x The object to unwrap.
#' @param msg The message to be included if a condition is thrown.
#'
#' @returns The value wrapped by `x` if a condition was not thrown as result of a `Failure` or `Nothing` value.
#'
#' @examples
#' Success(10) |> expect("This value should have been successfully computed")
#' Some("optional string") |> expect("Actually we require the string")
#' \dontrun{
#' Failure("Oh no I failed because of bad parameters") |> expect("This value should have been successfully computed")
#' Nothing() |> expect("Actually we require the string")
#' }
#'
#' @seealso [expect_fail()] to unwrap details of a `Failure` instead,
#'   [unwrap()] for a similar function that doesn't require `msg`.
#'
#' @export
expect <- S7::new_generic("expect", c("x", "msg"))

#' Get the wrapped error details from a `Failure` object
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' If the provided object is a `Success` then the function will 'panic'
#' and throw an error of class `expect_fail_panic`, with the message body set to
#' `msg`.
#'
#' @param x The object to unwrap.
#' @param msg The message to be included if a condition is thrown.
#'
#' @returns The error wrapped by `x` if a condition was not thrown as result of a `Success` value.
#'
#' @examples
#' Failure("Oh no I failed because of bad parameters") |> expect_fail("We have anticipated this failure")
#' \dontrun{
#' Success(10) |> expect_fail("No Mx. Bond, I expect you to fail!")
#' }
#'
#' @seealso [expect()] to unwrap the value of a `Success` instead,
#'   [unwrap_fail()] for a similar function that doesn't require `msg`.
#'
#' @export
expect_fail <- S7::new_generic("expect_fail", c("x", "msg"))

#' Get the wrapped value from a `Result` or `Option` object
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' If the provided object is a `Failure` (in the case of a `Result` input) or
#' `Nothing` (in the case of an `Option` input), then the function will 'panic'
#' and throw an error of class `unwrap_fail_panic`.
#'
#' For this reason, use of this function is not generally advised unless you
#' specifically want to crash your R program or throw an error condition.
#'
#' Instead you are encouraged to handle the "unhappy path" yourself using
#' `unwrap_or_default` or `unwrap_or_else`.
#'
#' @param x The object to unwrap.
#'
#' @returns The value wrapped by `x` if a condition was not thrown as result of a `Failure` or `Nothing` value.
#'
#' @examples
#' Success(10) |> unwrap()
#' Success(1:10) |> unwrap()
#' Success(c(TRUE, TRUE, FALSE)) |> unwrap()
#' \dontrun{
#' Failure("Ooopsy") |> unwrap()
#' }
#'
#' @seealso Prefer [unwrap_or_default()] or [unwrap_or_else()] to explicitly handle the unhappy path.
#'   Alternatively, use [unwrap_option()] to change a `Success` to a `Some` and a `Failure` to a `Nothing`.
#'
#' @export
unwrap <- S7::new_generic("unwrap", "x")

#' Get the wrapped error details from a `Failure` object
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' If the provided object is a `Success` then the function will 'panic'
#' and throw an error of class `expect_fail_panic`, with the message body set to
#' `msg`.
#'
#' @param x The object to unwrap.
#'
#' @returns The error wrapped by `x` if a condition was not thrown as result of a `Success` value.
#'
#' @examples
#' Failure("Oh no I failed because of bad parameters") |> expect_fail("We have anticipated this failure")
#' \dontrun{
#' Success(10) |> expect_fail("No Mx. Bond, I expect you to fail!")
#' }
#'
#' @seealso [unwrap()] to unwrap the value of a `Success` instead.
#'
#' @export
unwrap_fail <- S7::new_generic("unwrap_fail", "x")

#' Get the wrapped value from a `Result` or `Option` object
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' If the provided object is a `Failure` (in the case of a `Result` input) or
#' `Nothing` (in the case of an `Option` input), then the function will return
#' the provided default value instead.
#'
#' @param x The object to unwrap.
#' @param default The default value to return in the event that `x` is a `Failure`.
#'
#' @returns The value wrapped by a `Success` or `default` in the case of a `Failure`.
#'
#' @examples
#' Success(10) |> unwrap_or_default(200)
#' Failure("OMG I failed") |> unwrap_or_default(200)
#' Some(50) |> unwrap_or_default(200)
#' Nothing() |> unwrap_or_default(200)
#'
#' @seealso [unwrap_or_else()]
#'
#' @export
unwrap_or_default <- S7::new_generic("unwrap_or_default", c("x", "default"))

#' @export
unwrap_or_else <- S7::new_generic("unwrap_or_else", c("x", "fn"))

#' Get the wrapped value from a `Result`, wrapped in an `Option` object
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This is useful for converting a `Result` to an `Option` i.e. to go from
#' the possibility of an answer or details of a failure, to the possibility of
#' an answer or nothing.
#'
#' @param x A `Result` object i.e. a `Success()` or `Failure()` value.
#'
#' @returns `Some(x)` in the case of a `Success` where `x` is the unwrapped value of the `Success`, or `Nothing()` in the case of a `Failure()`.
#'
#' @export
unwrap_option <- S7::new_generic("unwrap_option", "x")

#' @export
unwrap_fail_option <- S7::new_generic("unwrap_fail_option", "x")

#' Process a success or pass through a failure
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Calls `fn()` on the wrapped value of `x` if `x` is a `Success`, or `Some`,
#' otherwise returns `x` if it is a `Failure` or `Nothing`.
#'
#' This function can be used for control flow based on Result values.
#'
#' @export
and_then <- S7::new_generic("and_then", c("x", "fn"))

#' Process a failiure or pass through a success
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Calls `fn()` on the wrapped value of `x` if `x` is a `Failure`, or `Nothing`,
#' otherwise returns `x` if it is a `Success` or `Some`.
#'
#' This function can be used for control flow based on Result values.
#'
#' @export
or_else <- S7::new_generic("or_else", c("x", "fn"))

#' @export
result <- S7::new_generic("result", "fn")

S7::method(result, S7::class_function) <- function(fn, detect_warning = TRUE, fail_on_warning = TRUE) {
  rlang::try_fetch(
    {
      wrapped_fn <- if (detect_warning) {
        function(...) {
          rlang::try_fetch(fn(...), error = identity, warning = identity) |>
            wrap_in_result(fail_on_warning = fail_on_warning)
        }
      } else {
        function(...) {
          rlang::try_fetch(fn(...), error = identity) |>
            wrap_in_result(fail_on_warning = fail_on_warning)
        }
      }
      class(wrapped_fn) <-c("resultr::result_wrapped_function", "resultr::wrapped_function", class(wrapped_fn))
      wrapped_fn
    },
    error = function(err) {
      panic("There was a problem wrapping a function", class = "resultr::result_function_wrap_panic", parent = err)
    }
  )
}

S7::method(result, S7::class_formula) <- function(fn, detect_warning = TRUE, fail_on_warning = TRUE) {
  fn <- purrr::as_mapper(fn)
  result(fn, detect_warning = detect_warning, fail_on_warning = fail_on_warning)
}

#' @include S3_classes.R
S7::method(result, class_result_wrapped_function) <- function(fn, detect_warning = TRUE, fail_on_warning = TRUE) {
  rlang::warn(
    message = "resultr: Attempting to wrap a Result wrapped function that is already wrapped",
    class = "resultr::result_function_already_wrapped_warning"
  )
  fn
}






