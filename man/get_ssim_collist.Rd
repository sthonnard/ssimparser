% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssimparser.R
\name{get_ssim_collist}
\alias{get_ssim_collist}
\title{get_ssim_collist}
\usage{
get_ssim_collist(getall = TRUE)
}
\arguments{
\item{getall}{Get all columns (TRUE/FALSE).}
}
\value{
Vector containing the SSIM columns.
}
\description{
Get the list of columns that can be parsed from SSIM.
}
\examples{
# Get all columns
get_ssim_collist()
# Get some of the most 'useful' columns
get_ssim_collist(FALSE)
}
