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
    return(suppressWarnings(as.character(airportr::airport_detail(iata)["ICAO"])))
  }, error = function(e){return("-")}, warning = function(w){}
  )
}

# Get a test SSIM file for validation
get_ssim_sample <- function(datefrom = as.Date("2020-11-01"), dateto = as.Date("2020-12-01"), season="W20")
{
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  creadate <- stringr::str_to_upper(format(Sys.Date(), "%d%b%y"))
  ssimdatefrom <- stringr::str_to_upper(format(datefrom, "%d%b%y"))
  ssimdateto <- stringr::str_to_upper(format(dateto, "%d%b%y"))

  ssim_sample <- paste0("1AIRLINE STANDARD SCHEDULE DATA SET     1                                                                                                                                                      000000001
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2LAF      W20 ",ssimdatefrom,ssimdateto,creadate, "SSIM EXAMPLE SCHEDULE        ",creadate,"CKENNY                               TEST AIRLINE                                                 1/8/13/18          ET1800000002
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3XAF 43310101J",ssimdatefrom,ssimdateto,"12345672CDG18451845+0100T1ALC20252020+01001F320XX                  XX   XX        XX XX    XXX      XX XX XX XX 1234   2L W                     X20D52X99XA320      00000003
4XAF 43310101J              XX020CDGALCAF TEST                                                                                                                                                    000004
4XAF 43310101J              XX021CDGALCAF TEST                                                                                                                                                    000005
4XAF 43310101J              XX026CDGALCAF TEST                                                                                                                                                    000006
3XAF 43310101J",ssimdatefrom,stringr::str_to_upper(format(min(dateto, datefrom + 21), "%d%b%y")),"     672CDG07000700+0100T1ALC08300830+01001F320XX                  XX   XX        XX XX    XXX      XX XX XX XX 1234   2L W                     X20D52X99XA320      00000007
3XAF 12340101J",ssimdatefrom,stringr::str_to_upper(format(min(dateto, datefrom + 15), "%d%b%y")),"     672CDG18451945+0100T1ALC21252120+01001F320XX                  XX   XX        XX XX    XXX      XX XX XX XX 1234   2L W                     X20D52X99XA320      00000008
")

  Sys.setlocale("LC_TIME", lct)
  return(ssim_sample)
}




