% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_ssimparser.R
\name{get_airport_icao}
\alias{get_airport_icao}
\title{get_airport_icao}
\usage{
get_airport_icao(iata)
}
\arguments{
\item{iata}{Airport IATA code.}
}
\value{
A character vector containing the airport ICAO or '-' when \code{\link[airportr:airport_detail]{airportr::airport_detail}} returned an error.
}
\description{
Get airport ICAO with package airportr.
}
\keyword{internal}
