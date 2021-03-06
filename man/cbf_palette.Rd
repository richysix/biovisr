% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbf_palette.R
\name{cbf_palette}
\alias{cbf_palette}
\title{A colour-blind friendly colour palette}
\usage{
cbf_palette(palette_size = 8, named = FALSE)
}
\arguments{
\item{palette_size}{numeric    number of colours required}

\item{named}{logical    should the vector be named. default = FALSE}
}
\value{
named vector of colours
}
\description{
\code{cbf_palette} returns a colour friendly palette
}
\details{
Returns a vector of the specified length. If the supplied length is
greater than 8, a warning is generated and a vector of length 8 is returned.
If no length is supplied the whole palette of 8 colours is returned.
This palette is intended for categorical data.
}
\examples{
palette <- cbf_palette()

palette <- cbf_palette(4)

}
