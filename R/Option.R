
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
