% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{find_row_and_column_spaces}
\alias{find_row_and_column_spaces}
\title{Function: Row and column spaces}
\usage{
find_row_and_column_spaces(A)
}
\arguments{
\item{A}{matrix of real numbers.}
}
\value{
Returns a list containing the matrix \code{A} reduced by rows and the rows and columns vectors.
}
\description{
Performs Gauss Elimination with Partial Pivoting of A and returns a list containing the row and column vectors of the given matrix
}
\examples{
B <- matrix(sample.int(10, 12, replace = TRUE),  nrow = 3)
find_row_and_column_spaces(B)
A <- matrix(c(1,  1,
              1, -1,
              1,  0,
              1,  0), byrow = TRUE, ncol = 2)
find_row_and_column_spaces(A)
}
