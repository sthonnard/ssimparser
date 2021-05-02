#' get_utc_time
#'
#' Get UTC date and time from SSIM local date time and offset.
#' @param pdate  Local date formatted as \%d\%b\%y" (eg 27NOV20).
#' @param ptime  Local time formatted \%H\%M (eg 1345 for 13:45).
#' @param poffset  UTC offset (+0130 for +01:30).
#'
#' @return A "POSIXct" or the description of a conversion problem as a character vector.
#' @keywords internal
get_utc_time <- function(pdate = "27NOV20", ptime = "0000", poffset = "+0000")
{

  tryCatch(
    {
      lct <- Sys.getlocale("LC_TIME")
      Sys.setlocale("LC_TIME", "C")
      local_time_char <- paste0(format(as.Date(pdate, "%d%b%y"),"%Y-%m-%d")," ", stringr::str_sub(ptime,1,2),":",stringr::str_sub(ptime,3,4),":00")
      local_date_time <- as.POSIXct(local_time_char, tz = "UTC")
      Sys.setlocale("LC_TIME", lct)

      tryCatch(
        {
          # substract the offset to get the utc time
          if (stringr::str_sub(poffset,1,1) == "+")
          {
            return(local_date_time - as.double( stringr::str_sub(poffset,2,3))*60*60 - as.double( stringr::str_sub(poffset,4,5))*60)
          }else
          {
            return(local_date_time + as.double( stringr::str_sub(poffset,2,3))*60*60 + as.double( stringr::str_sub(poffset,4,5))*60)
          }
        },
        error = function(e){return(paste("error when applying offset", poffset,"to",local_time_char ))}
      )
    },
    error = function(e){return(paste("error converting", local_time_char,"to POSIXct" ))}
  )

}

#' get_day_of_week
#'
#' Get day of week (1 = Monday).
#' @param pdate  An object of class "POSIXct" from which day of week will be extracted.
#'
#' @return A double representing the day of week.
#' @keywords internal
get_day_of_week <- function(pdate = Sys.Date())
{
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  d <- base::weekdays(pdate, abbreviate = FALSE)
  Sys.setlocale("LC_TIME", lct)
  return(switch(d, "Monday" = 1,
                "Tuesday" = 2,
                "Wednesday" = 3,
                "Thursday" = 4,
                "Friday" = 5,
                "Saturday" = 6,
                "Sunday" = 7
  ))
}

#' get_airport_icao
#'
#' Get airport ICAO with package airportr.
#' @param iata  Airport IATA code.
#'
#' @return A character vector containing the airport ICAO or '-' when \code{\link[airportr:airport_detail]{airportr::airport_detail}} returned an error.
#' @keywords internal
get_airport_icao <- function(iata)
{
  tryCatch({
    return(suppressWarnings(as.character(airportr::airport_detail(iata)["ICAO"])))
  }, error = function(e){return("-")}, warning = function(w){}
  )
}
