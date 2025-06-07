
#' @export
inspect <- S7::new_generic("inspect", "x", function(x, fn, ...) {
  S7::S7_dispatch()
})

#' @export
inspect_fail <- S7::new_generic("inspect_fail", "x", function(x, fn, ...) {
  S7::S7_dispatch()
})

#' @export
expect <- S7::new_generic("expect", c("x", "msg"))

#' @export
expect_fail <- S7::new_generic("expect_fail", c("x", "msg"))

#' Get the wrapped value from a `Result` or `Option` object
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
#' @seealso [unwrap_or_default()] [unwrap_or_else()]
#'
#' @export
unwrap <- S7::new_generic("unwrap", "x")

#' @export
unwrap_fail <- S7::new_generic("unwrap_fail", "x")

#' Get the wrapped value from a `Result` or `Option` object
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
#' @seealso [unwrap()] [unwrap_or_else()]
#'
#' @export
unwrap_or_default <- S7::new_generic("unwrap_or_default", c("x", "default"))

#' @export
unwrap_option <- S7::new_generic("unwrap_option", "x")

#' @export
unwrap_fail_option <- S7::new_generic("unwrap_fail_option", "x")

#' @export
and_then <- S7::new_generic("and_then", c("x", "fn"))

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






