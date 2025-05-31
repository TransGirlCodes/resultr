#' @export
panic <- function(message, class, ...) {
  rlang::abort(
    message = message,
    class = paste0("resultr::", c(class, "panic")),
    ...
  )
}
