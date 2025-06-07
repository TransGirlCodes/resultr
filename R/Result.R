
# Result Class ------------------------------------------------------------

#' The abstract parent class of the `Success` and `Failure` classes
#'
#' As an `S7` abstract class. This can't actually be constructed, but can be used
#' for the purposes of dispatch.
#'
#' @export
Result <- S7::new_class(
  "Result",
  abstract = TRUE
)

#' A class for representing the value of a successful computation
#'
#' The `Result` class wraps a value in order to provide context to the value.
#' Namely, that the value is the result of a successful computation. i.e. a
#' function finished without an issue or error.
#'
#' @param value The object to wrap - typically the result that would be returned by of one of your functions.
#' @returns A `Success` `S7` object with its `@value` property set to the `value` that was passed in the constructor.
#' @examples
#' Success(10)
#' Success(1:10)
#' Success(TRUE)
#' Success(data.frame(a = 1:10, b = 10:1))
#'
#' @seealso [Failure()], which wraps a value to provide the context of a failed computation.
#' @export
Success <- S7::new_class(
  "Success",
  parent = Result,
  properties = list(
    value = S7::new_property(
      class = S7::class_any,
      setter = function(self, value) {
        if (!is.null(self@value)) {
          stop("@value is read-only")
        }
        self@value <- value
        self
      }
    )
  )
)

#' @export
Failure <- S7::new_class(
  "Failure",
  parent = Result,
  properties = list(
    error = S7::new_property(
      class = S7::class_any,
      setter = function(self, value) {
        if (!is.null(self@error)) {
          stop("@error is read-only")
        }
        self@error <- value
        self
      }
    )
  )
)

#' @export
is_result <- function(x) { S7::S7_inherits(x, class = Result) }

check_is_result <- function(x) {
  if(!is_result(x)) {
    panic("value returned from function must be a Result", class = "result_check_panic")
  } else {
    x
  }
}

#' @export
is_success <- function(x) { S7::S7_inherits(x, class = Success) }

#' @export
is_failure <- function(x) { S7::S7_inherits(x, class = Failure) }

#' @export
is_success_and <- function(x, fn) {
  fn <- purrr::as_mapper(fn)
  is_success(x) && fn(x)
}

#' @export
is_failure_and <- function(x, fn) {
  fn <- purrr::as_mapper(fn)
  is_failure(x) && fn(x)
}

#' @include Generics.R
S7::method(inspect, Success) <- function(x, fn) {
  fn <- purrr::as_mapper(fn)
  fn(x@value)
  x
}

S7::method(inspect, Failure) <- function(x, fn) { x }

S7::method(inspect_fail, Success) <- function(x, fn) { x }

S7::method(inspect_fail, Failure) <- function(x, fn) {
  fn <- purrr::as_mapper(fn)
  fn(x@error)
  x
}

S7::method(expect, list(Success, S7::class_character)) <- function(x, msg) { x@value }

S7::method(expect, list(Failure, S7::class_character)) <- function(x, msg) {
  panic(
    message = c("An expect has been violated", msg),
    fail_value = x,
    class = "expect_panic"
  )
}

S7::method(expect_fail, list(Success, S7::class_character)) <- function(x, msg) {
  panic(
    message = c(
      "An expect_err has been violated",
      msg
    ),
    success_value = x,
    class = "expect_fail_panic"
  )
}

S7::method(expect_fail, list(Failure, S7::class_character)) <- function(x, msg) { x@error }

S7::method(unwrap, Success) <- function(x) { x@value }

S7::method(unwrap, Failure) <- function(x) {
  panic(
    "called `unwrap()` on a `Failure` value",
    fail_value = x,
    class = "unwrap_panic"
  )
}

S7::method(unwrap_or_default, list(Success, S7::class_any)) <- function(x, default) { x@value }
S7::method(unwrap_or_default, list(Failure, S7::class_any)) <- function(x, default) { default }

#' @include Option.R
S7::method(unwrap_option, Success) <- function(x) { Some(x@value) }
S7::method(unwrap_option, Failure) <- function(x) { Nothing() }

S7::method(unwrap_fail, Success) <- function(x) {
  panic(
    "called `unwrap_fail()` on a `Success` value",
    success_value = x,
    class = "unwrap_fail_panic"
  )
}

S7::method(unwrap_fail, Failure) <- function(x) { x@error }

#' @include S3_classes.R
#' @export
wrap_in_result <- S7::new_generic("wrap_in_result", "x")

S7::method(wrap_in_result, Result) <- function(x, ...) { x }
S7::method(wrap_in_result, S7::class_any) <- function(x, ...) { Success(x) }
S7::method(wrap_in_result, class_error) <- function(x, ...) { Failure(x) }
S7::method(wrap_in_result, class_warning) <- function(x, fail_on_warning = TRUE, ...) {
  if (isTRUE(fail_on_warning)) { Failure(x) } else { Success(x) }
}

S7::method(
  and_then,
  list(Success, S7::class_formula | S7::class_function)
) <- function(x, fn, detect_warning = TRUE, fail_on_warning = TRUE) {
  and_then(
    x,
    result(
      fn,
      detect_warning = detect_warning,
      fail_on_warning = fail_on_warning
    )
  )
}

S7::method(
  and_then,
  list(Success, class_result_wrapped_function)
) <- function(x, fn) {
  x@value |> fn()
}

S7::method(and_then, list(Failure, S7::class_any)) <- function(x, fn) { x }


