
# Result Class ------------------------------------------------------------

#' The abstract parent class of the `Success` and `Failure` classes
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' As an `S7` abstract class. This can't actually be constructed, but can be used
#' for the purposes of dispatch.
#'
#' @rdname result-class
#' @export
Result <- S7::new_class(
  "Result",
  abstract = TRUE
)

#' A class for representing the value of a successful computation
#'
#' @description
#' `r lifecycle::badge('stable')`
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

#' A class for representing a failed or stopped computation
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @examples
#' Failure("The computation failed")
#' Failure("Argument x must be numeric")
#' Failure(rlang::catch_cnd(stop("An error was thrown")))
#'
#' @param error An error value to wrap. Could be a numeric error code, a string with a message or a condition object.
#'
#' @seealso [Success()], which wraps a value to provide the context of a successful computation.
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

#' Check if an object is a subclass of `Result`
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The object to test.
#'
#' @returns Either `TRUE` or `FALSE`.
#'
#' @examples
#' Success(10) |> is_result()
#' Failure("OOps!") |> is_result()
#' Some(10) |> is_result()
#' Nothing() |> is_result()
#'
#' @seealso [check_is_result()]
#'
#' @export
is_result <- function(x) { S7::S7_inherits(x, class = Result) }

#' Check if an object is a subclass of `Result`
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The object to test.
#'
#' @returns Either `TRUE` or `FALSE`.
#'
#' @examples
#' Success(10) |> is_result()
#' Failure("OOps!") |> is_result()
#' Some(10) |> is_result()
#' Nothing() |> is_result()
#'
#' @seealso [is_result()]
#'
#' @export
check_is_result <- function(x) {
  if(!is_result(x)) {
    panic("value returned from function must be a Result", class = "result_check_panic")
  } else {
    x
  }
}

#' Check if an object is a subclass of `Success`
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The object to test.
#'
#' @returns Either `TRUE` or `FALSE`.
#'
#' @examples
#' Success(10) |> is_success()
#' Failure("OOps!") |> is_success()
#' Some(10) |> is_success()
#' Nothing() |> is_success()
#'
#' @seealso [is_failure()]
#'
#' @export
is_success <- function(x) { S7::S7_inherits(x, class = Success) }

#' Check if an object is a subclass of `Failure`
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The object to test.
#'
#' @returns Either `TRUE` or `FALSE`.
#'
#' @examples
#' Success(10) |> is_failure()
#' Failure("OOps!") |> is_failure()
#' Some(10) |> is_failure()
#' Nothing() |> is_failure()
#'
#' @seealso [is_success()]
#'
#' @export
is_failure <- function(x) { S7::S7_inherits(x, class = Failure) }

#' Check if an object is a subclass of `Success` AND a predicate is true
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The object to test.
#' @param fn A single argument predicate function that accepts `x` as its argument.
#'
#' @returns Either `TRUE` or `FALSE`.
#'
#' @examples
#' Success(10) |> is_success_and(~ unwrap(.x) == 10)
#' Failure("OOps!") |> is_success_and(~ unwrap(.x) == 10)
#' Some(10) |> is_success_and(~ unwrap(.x) == 10)
#' Nothing() |> is_success_and(~ unwrap(.x) == 10)
#'
#' @seealso [is_failure_and()]
#'
#' @export
is_success_and <- function(x, fn) {
  fn <- purrr::as_mapper(fn)
  is_success(x) && fn(x)
}

#' Check if an object is a subclass of `Failure` AND a predicate is true
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @param x The object to test.
#' @param fn A single argument predicate function that accepts `x` as its argument.
#'
#' @returns Either `TRUE` or `FALSE`.
#'
#' @examples
#' Success(10) |> is_failure_and(~ unwrap_fail(.x) == "OOps!")
#' Failure("OOps!") |> is_failure_and(~ unwrap_fail(.x) == "OOps!")
#' Some(10) |> is_failure_and(~ unwrap_fail(.x) == "OOps!")
#' Nothing() |> is_failure_and(~ unwrap_fail(.x) == "OOps!")
#'
#' @seealso [is_success_and()]
#'
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

S7::method(unwrap_or_else, list(Success, S7::class_function)) <- function(x, fn) { x@value }
S7::method(unwrap_or_else, list(Failure, S7::class_function)) <- function(x, fn) { x@error |> fn() }



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

#' Wrap a value in a `Result` object depending on its class.
#'
#' This function is an important helper function for the `result` function
#' wrapper.
#'
#' When a wrapped `fn` successfully computes and returns a value, it is wrapped
#' into a `Result` object according to the following rules.
#'
#' A value of any class (i.e. `S7::class_any`) is wrapped in a `Success` by default.
#' Errors and Warnings (i.e. `S7::class_error | S7::class_warning`) are wrapped in a `Failure` by default
#' A `Result` is passed through as is - no point wrapping a `Result` in a `Result`.
#'
#' These rules are implemented through this function's method dispatch, and so
#' class authors can provide methods to dictate how their specific class should
#' be wrapped. However it is expected that the simple rules already defined should
#' be sufficient for the vast majority of cases.
#'
#' @param x The object to be wrapped into a `Result`.
#' @param ... Additional optional arguments that may be used by individual methods.
#'
#' @seealso [result()]
#'
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
  list(
    Success,
    S7::class_formula | S7::class_function
  )
) <- function(x, fn, detect_warning = TRUE, fail_on_warning = TRUE) {
  if (detect_warning) {
    rlang::try_fetch(
      { fn(x@value) },
      error = identity,
      warning = identity
    ) |> wrap_in_result(fail_on_warning = fail_on_warning)
  } else {
    rlang::try_fetch(
      { fn(x@value) },
      error = identity,
    ) |> wrap_in_result(fail_on_warning = fail_on_warning)
  }
}

S7::method(
  and_then,
  list(Success, class_result_wrapped_function)
) <- function(x, fn) {
  x@value |> fn()
}

S7::method(and_then, list(Failure, S7::class_any)) <- function(x, fn) { x }


