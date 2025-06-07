
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
