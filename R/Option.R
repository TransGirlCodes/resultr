
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
