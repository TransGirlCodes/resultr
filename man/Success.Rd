% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Result.R
\name{Success}
\alias{Success}
\title{A class for representing the value of a successful computation}
\usage{
Success(value = NULL)
}
\arguments{
\item{value}{The object to wrap - typically the result that would be returned by of one of your functions.}
}
\value{
A \code{Success} \code{S7} object with its \verb{@value} property set to the \code{value} that was passed in the constructor.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

The \code{Result} class wraps a value in order to provide context to the value.
Namely, that the value is the result of a successful computation. i.e. a
function finished without an issue or error.
}
\examples{
Success(10)
Success(1:10)
Success(TRUE)
Success(data.frame(a = 1:10, b = 10:1))

}
\seealso{
\code{\link[=Failure]{Failure()}}, which wraps a value to provide the context of a failed computation.
}
