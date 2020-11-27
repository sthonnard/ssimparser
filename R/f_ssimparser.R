# get UTC date and time from SSIM local date time and offset
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

# Get day of week (1=Monday)
get_day_of_week <- function(pdate = Sys.Date())
{
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  d <- base::weekdays(pdate)
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

# Get airport ICAO with airportr
get_airport_icao <- function(iata)
{
  tryCatch({
    return(as.character(airportr::airport_detail(iata)["ICAO"]))
  }, error = function(e){return("-")}
  )
}

