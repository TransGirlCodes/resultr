% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Generics.R
\name{unwrap_fail}
\alias{unwrap_fail}
\title{Get the wrapped error details from a \code{Failure} object}
\usage{
unwrap_fail(x, ...)
}
\arguments{
\item{x}{The object to unwrap.}

\item{...}{Additional optional arguments that may be used by specific methods.}
}
\value{
The error wrapped by \code{x} if a condition was not thrown as result of a \code{Success} value.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

If the provided object is a \code{Success} then the function will 'panic'
and throw an error of class \code{expect_fail_panic}, with the message body set to
\code{msg}.
}
\examples{
Failure("Oh no I failed because of bad parameters") |> unwrap_fail()
\dontrun{
Success(10) |> unwrap_fail()
}

}
\seealso{
\code{\link[=unwrap]{unwrap()}} to unwrap the value of a \code{Success} instead.
}
