% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/reshape_long.R
\name{reshape_long}
\alias{reshape_long}
\title{Reshape to Long}
\usage{
reshape_long(x, idvar = "ID")
}
\arguments{
\item{x}{A \code{\link[base]{data.frame}} in wide format.}
}
\value{
Returns a \code{\link[base]{data.frame}}.
}
\description{
Reshape \code{\link[base]{data.frame}} from wide to long format.
}
\examples{
x <- r_data_frame(
        n = 500,
        id,
        age, age, age,
        grade, grade, grade
)
reshape_long(x)
library(dplyr)
x <- r_data_frame(
        n = 500,
        id,
        age,
        sex,
        r_series(date_stamp, 5, name = "Date"),
        r_series(likert, 5, name = "Question")
) \%>\%
    reshape_long()
}

