% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Option.R
\name{Some}
\alias{Some}
\title{An object to represent "some value".}
\usage{
Some(value = NULL)
}
\arguments{
\item{value}{The value that is wrapped by the \code{Some} object.}
}
\description{
"Some value" here means a value wrapped in a \code{Some} object, as opposed to a
\code{Nothing} object.
}
\details{
Note R has other means by which an author might represent missing-ness or
ambiguity like \code{NULL}, \code{NA}s and 0-length vectors.
}
\examples{
Some(100)
Some("Some Text")
Some(1:10)
Some(TRUE)
Some(FALSE)

}
