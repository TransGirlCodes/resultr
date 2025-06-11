
# Option class ------------------------------------------------------------

#' The abstract parent class of the `Some` and `Nothing` classes
#'
#' As an `S7` abstract class. This can't actually be constructed, but can be used
#' for the purposes of dispatch.
#'
#' @export
Option <- S7::new_class(
  "Option",
  abstract = TRUE
)

#' An object to represent "some value".
#'
#' "Some value" here means a value wrapped in a `Some` object, as opposed to a
#' `Nothing` object.
#'
#' Note R has other means by which an author might represent missing-ness or
#' ambiguity like `NULL`, `NA`s and 0-length vectors.
#'
#' @param value The value that is wrapped by the `Some` object.
#'
#' @examples
#' Some(100)
#' Some("Some Text")
#' Some(1:10)
#' Some(TRUE)
#' Some(FALSE)
#'
#' @export
Some <- S7::new_class(
  "Some",
  parent = Option,
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

#' A class to represent no-value or nothing.
#'
#' @examples
#' Nothing()
#'
#' @export
Nothing <- S7::new_class(
  "Nothing",
  parent = Option
)

S7::method(expect, list(Some, S7::class_character)) <- function(x, msg) { x@value }

S7::method(expect, list(Nothing, S7::class_character)) <- function(x, msg) {
  panic(
    message = c("An expect has been violated", msg),
    fail_value = x,
    class = "expect_panic"
  )
}

S7::method(unwrap, Some) <- function(x) { x@value }

S7::method(unwrap, Nothing) <- function(x) {
  panic(
    "called `unwrap()` on a `Nothing` value",
    fail_value = x,
    class = "unwrap_panic"
  )
}

S7::method(unwrap_or_default, list(Some, S7::class_any)) <- function(x, default) { x@value }
S7::method(unwrap_or_default, list(Nothing, S7::class_any)) <- function(x, default) { default }
