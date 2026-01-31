#' ssimparser: A Tool for Parsing Standard Schedules Information (Chapter 7).
#'
#' Parse SSIM file (types 2 and 3) into a Data Frame.\cr\cr
#'  Bugs report:\cr
#'  \url{https://github.com/sthonnard/ssimparser}
#'
#'
#' @section ssimparser functions:
#' \strong{get_ssim_collist()}\cr
#' Get the list of columns that can be parsed from SSIM.\cr
#'
#' \strong{load_ssim(ssim_file)}\cr
#' Parse SSIM file into a Data Frame.\cr
#'
#' \strong{load_ssim_flights(ssim_files)}\cr
#' Parse multiple SSIM files, expand to flights, and return the result into a Data Frame.\cr
#'
#' \strong{get_ssim_sample()}\cr
#' Get a sample SSIM file as a character vector.\cr
#'
#' @docType package
#' @name ssimparser
#'
#'
#' @keywords internal
"_PACKAGE"
