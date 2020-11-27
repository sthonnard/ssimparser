#' ssimparser: A Tool For Parsing SSIM Schedules.
#'
#' This package parses SSIM Schedules (type 2 and 3) into a Data Frame.
#'  Bugs report:\cr
#'  \url{https://github.com/sthonnard/ssimparser}
#'
#'
#' @section ssimparser functions:
#' \strong{get_ssim_collist()}\cr
#' Get the list of columns that can be parsed from SSIM Schedules\cr
#'
#' \strong{load_ssim(ssim_file)}\cr
#' Parse SSIM file into a Data Frame.\cr
#'
#' \strong{load_ssim_files(ssim_file)}\cr
#' Parse multiple SSIM files, convert to flights,  and resturn the result into a Data Frame.\cr
#'
#' @docType package
#' @name ssimparser
#'
#'

source("./R/f_ssimparser.R")

#' get_ssim_collist
#'
#' Get the list of columns that can be parsed from SSIM Schedules
#' @param getall  Get all columns (TRUE/FALSE).
#'
#' @return Vector containing the columns
#' @export
#'
#' @examples
#' # Get all columns
#' get_ssim_collist()
#' # Get some of the most 'useful' columns
#' get_ssim_collist(FALSE)
get_ssim_collist <- function(getall=TRUE)
{
  if (getall)
  {

    return(c("type2.timemode", "type2.iata_airline", "type2.season",
             "type2.valid_from",
             "type2.valid_to",                        "type2.creation_date",                    "type2.title_of_data",
             "type2.release_date",                     "type2.schedule_status",                  "type2.creator_reference",
             "type2.duplicate_airline_designator",     "type2.general_information",              "type2.inflight_service_information",
             "type2.electronic_ticketing_information", "type2.creation_time",      "n_type3",     "type3.operational_suffix",
             "type3.airline_designator",              "type3.flight_number",                    "type3.itinerary_variation",
             "type3.leg_sequence",                     "type3.service_type",                     "type3.period_from",
             "type3.period_to",                        "type3.days_of_operation",                "type3.frequency_rate",
             "type3.adep_iata",                        "type3.passenger_std",                    "type3.std",
             "type3.adep_utc_offset",                  "type3.adep_terminal",                    "type3.ades_iata",
             "type3.sta",                              "type3.passenger_sta",                    "type3.ades_utc_offset",
             "type3.ades_terminal",                    "type3.aircraft_type_iata",               "type3.prbd",
             "type3.meal_service",                     "type3.joint_operation",                  "type3.minimum_connecting_time_status",
             "type3.secure_flight_indicator",                                    "type3.itinerary_variation_overflow",
             "type3.aircraft_owner",                   "type3.cockpit_crew_employer",            "type3.cabin_crew_employer",
             "type3.onward_flight_operator_iata",      "type3.onward_flight_number",             "type3.aircraft_rotation_layover",
             "type3.onward_operation_suffix",                                   "type3.flight_transit_layower",
             "type3.code_sharing",                     "type3.traffic_restriction_code",         "type3.traffic_restriction_code_leg",
             "type3.aircraft_configuration",           "type3.date_variation",

             # Not in the SSIM Schedules file but derived during the loading:
             "type3.std_utc","type3.sta_utc", "flight.flight_date", "type3.adep_icao", "type3.ades_icao"
    ))
  }
  else
  {
    return(c("type2.iata_airline","type3.flight_number", "type3.service_type", "type3.period_from", "type3.period_to",
             "type3.days_of_operation", "type3.adep_iata", "type3.ades_iata",
             "type3.aircraft_type_iata", "type3.code_sharing",

             "type3.std_utc", "type3.sta_utc", "flight.flight_date", "type3.adep_icao", "type3.ades_icao"))
  }
}

#' load_ssim
#'
#' Load SSIM file into a Data Frame.
#' @param ssim_file  Path to the SSIM file.
#' @param nestres  Nest SSIM type 3 into type 2 (TRUE/FALSE). Default to FALSE.
#' @param collist  List of columns that need to be present in the final Data Frame. get_ssim_collist() to get the full list.
#' @param cleannames  Clean column names in the final Data Frame by removing type2/type3 prefixes (TRUE/FALSE). Default TRUE.
#' @param punpivot  Unpivot the schedules by creating a schedule by day of operation (TRUE/FALSE). Default FALSE.
#' @param pexpand  Instantiate flights from the schedules.
#'
#' @return Data Frame (nested or not) containing the schedules (or instantiated flights).
#' @export
#'
#' @examples
#' # Load SSIM file
#' ssim <- load_ssim("./AFR_20201115.txt")
load_ssim <- function(ssim_file = "AUA_20201022.txt", nestres = FALSE, collist = get_ssim_collist(getall = FALSE),
                      cleannames=TRUE,
                      punpivot = TRUE,
                      pexpand = FALSE)
{

  ssimdf <- data.frame()

  con <- base::file(ssim_file, "r")
  ssim <- base::strsplit(base::readLines(con),"\n")

  print(paste(ssim_file,"has",length(ssim),"rows"))
  if (length(ssim) == 0){stop(paste(ssim_file, "is empty!"))}
  ssimdf <- as.data.frame(matrix(ssim, ncol = 1, byrow = TRUE))
  colnames(ssimdf) <- "lin"
  if (!(stringr::str_sub(ssimdf$lin[1],1,35) == "1AIRLINE STANDARD SCHEDULE DATA SET")){
    stop(paste(ssim_file,"is not a valid file airline standard schedule data set!"))
  }

  print(paste("dataframe has",nrow(ssimdf),"rows"))
  rm(ssim)
  close(con)

  ssimdf %>% dplyr::filter(stringr::str_sub(lin, 1, 1) == "2") %>%
    dplyr::mutate(
      type2.timemode = stringr::str_sub(lin, 2, 2),
      type2.iata_airline = stringr::str_trim(stringr::str_sub(lin,3,5)),
      type2.blank1 = stringr::str_sub(lin,6,10),
      type2.season = stringr::str_trim(stringr::str_sub(lin,11,13)),
      type2.blank2 = stringr::str_sub(lin,14,14),
      type2.valid_from = stringr::str_sub(lin,15,21),
      type2.valid_to = stringr::str_sub(lin,22,28),
      type2.creation_date = stringr::str_sub(lin,29,35),
      type2.title_of_data = stringr::str_sub(lin,36,64),
      type2.release_date = stringr::str_trim(stringr::str_sub(lin,65,71)),
      type2.schedule_status = stringr::str_sub(lin,72,72),
      type2.creator_reference = stringr::str_trim(stringr::str_sub(lin,73,107)),
      type2.duplicate_airline_designator = stringr::str_trim(stringr::str_sub(lin,108,108)),
      type2.general_information = stringr::str_trim(stringr::str_sub(lin,109,169)),
      type2.inflight_service_information = stringr::str_trim(stringr::str_sub(lin,170,188)),
      type2.electronic_ticketing_information = stringr::str_sub(lin,189,190),
      type2.creation_time = stringr::str_sub(lin,191,194), #HH24MI
      type2.record_serial_number = as.numeric(stringr::str_sub(lin,195,1200))
    ) %>% dplyr::select(-lin) %>%
    dplyr::arrange(type2.record_serial_number) %>%
    dplyr::mutate(type2.next_record_serial_number = dplyr::lead(type2.record_serial_number, 1)) -> type2

  ssimdf %>% dplyr::filter(stringr::str_sub(lin, 1, 1) == "3") %>%
    dplyr::mutate(
      type3.operational_suffix = stringr::str_trim(stringr::str_sub(lin, 2, 2)),
      type3.airline_designator = stringr::str_trim(stringr::str_sub(lin, 3, 5)), # as in type2.iata_airline
      type3.flight_number = stringr::str_trim(stringr::str_sub(lin, 6, 9)),
      type3.itinerary_variation = stringr::str_sub(lin, 10, 11),
      type3.leg_sequence = stringr::str_sub(lin, 12, 13),
      type3.service_type = stringr::str_sub(lin, 14, 14),
      type3.period_from = stringr::str_sub(lin, 15, 21), # might be encoded in local time, depending type 2 time mode
      type3.period_to = stringr::str_sub(lin, 22, 28), # might be encoded in local time, depending type 2 time mode
      type3.days_of_operation = stringr::str_trim(stringr::str_sub(lin, 29, 35)), # might be encoded in local time, depending type 2 time mode
      type3.frequency_rate = stringr::str_trim(stringr::str_sub(lin, 36, 36)),
      type3.adep_iata = stringr::str_sub(lin, 37, 39),
      type3.passenger_std = stringr::str_sub(lin, 40, 43),
      type3.std = stringr::str_sub(lin, 44, 47),
      type3.adep_utc_offset = stringr::str_sub(lin, 48, 52),
      type3.adep_terminal = stringr::str_trim(stringr::str_sub(lin, 53, 54)),
      type3.ades_iata = stringr::str_sub(lin, 55, 57),
      type3.sta = stringr::str_sub(lin, 58, 61),
      type3.passenger_sta = stringr::str_sub(lin, 62, 66),
      type3.ades_utc_offset = stringr::str_sub(lin, 66, 70),
      type3.ades_terminal = stringr::str_trim(stringr::str_sub(lin, 71, 72)),
      type3.aircraft_type_iata = stringr::str_sub(lin, 73, 75),
      type3.prbd = stringr::str_trim(stringr::str_sub(lin, 76, 95)), #Passenger Reservations Booking Designator
      type3.meal_service = stringr::str_sub(lin, 101, 110),
      type3.joint_operation = stringr::str_trim(stringr::str_sub(lin, 111, 119)),
      type3.minimum_connecting_time_status = stringr::str_sub(lin, 120, 121),
      type3.secure_flight_indicator = stringr::str_trim(stringr::str_sub(lin, 122, 122)),
      type3.blank1 = stringr::str_sub(lin, 123, 127),
      type3.itinerary_variation_overflow = stringr::str_trim(stringr::str_sub(lin, 128, 128)),
      type3.aircraft_owner = stringr::str_trim(stringr::str_sub(lin, 129, 131)),
      type3.cockpit_crew_employer = stringr::str_sub(lin, 132, 134),
      type3.cabin_crew_employer = stringr::str_sub(lin, 135, 137),
      type3.onward_flight_operator_iata = stringr::str_sub(lin, 138, 140),
      type3.onward_flight_number = stringr::str_sub(lin, 141, 144),
      type3.aircraft_rotation_layover = stringr::str_sub(lin, 145, 145),
      type3.onward_operation_suffix = stringr::str_sub(lin, 146, 146),
      type3.blank2 = stringr::str_sub(lin, 147, 147),
      type3.flight_transit_layower = stringr::str_sub(lin, 148, 148),

      type3.code_sharing = stringr::str_sub(lin, 149, 149),
      type3.traffic_restriction_code = stringr::str_sub(lin, 150, 160),
      type3.traffic_restriction_code_leg = stringr::str_sub(lin, 161, 161),
      type3.blank3 = stringr::str_sub(lin, 162, 172),
      type3.aircraft_configuration = stringr::str_sub(lin, 173, 192),
      type3.date_variation = stringr::str_sub(lin, 193, 194),
      type3.record_serial_number = as.numeric(stringr::str_sub(lin, 195, 200))
    ) %>% dplyr::select(-lin) %>%
    dplyr::arrange(type3.record_serial_number) %>%
    dplyr::mutate(type3.next_record_serial_number = dplyr::lead(type3.record_serial_number, 1)) -> type3

  # Get airport ICAO
  type3 %>% inner_join(
                        type3 %>% select(type3.adep_iata) %>%
                                  unique() %>%
                                  rowwise() %>%
                                  mutate(type3.adep_icao = get_airport_icao(type3.adep_iata)), by = c("type3.adep_iata")
  ) %>% inner_join(
                      type3 %>% select(type3.ades_iata) %>%
                        unique() %>%
                        rowwise() %>%
                        mutate(type3.ades_icao = get_airport_icao(type3.ades_iata)), by = c("type3.ades_iata")
  ) -> type3


  if (punpivot)
  { # unpivot type3.days_of_operation
    type3 %>% dplyr::group_by_all() %>%
      tidyr::expand(day_of_operation = seq(1:7)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(day_of_operation = as.character(day_of_operation)) %>%
      dplyr::filter(stringr::str_detect(type3.days_of_operation, day_of_operation ) > 0) %>%
      dplyr::mutate(type3.days_of_operation = day_of_operation) %>%
      dplyr::ungroup() %>%
      dplyr::select(-day_of_operation) -> type3
  }

  # Add type 2 serial number when missing, as it is for last record
  type2$type2.next_record_serial_number <- ifelse(is.na( type2$type2.next_record_serial_number),max(type3$type3.record_serial_number)+1, type2$type2.next_record_serial_number)

  # get n type3 for each type2
  type2 %>% dplyr::mutate(n_type3 = base::nrow(type3[which(type3$type3.record_serial_number > type2.record_serial_number &
                                                type3$type3.record_serial_number < type2.next_record_serial_number),])
  ) -> type2

  # Find the type 2 rownum for each type 3
  type3 %>% dplyr::rowwise() %>% dplyr::mutate(type3.type2_record_serial_number = base::max(type2[which(as.numeric(type2$type2.record_serial_number)<as.numeric(type3.record_serial_number)),]$type2.record_serial_number)) -> type3

  # Join type2 with type3 and nest the type3
  type2 %>% dplyr::left_join(type3, by=c("type2.record_serial_number"="type3.type2_record_serial_number")) -> ssimjoin


  # Convert from local to utc
  ssimjoin %>% dplyr::rowwise() %>%
    dplyr::mutate(type3.std_utc = get_utc_time(type3.period_from, type3.std, type3.adep_utc_offset),
           type3.sta_utc = get_utc_time(type3.period_from, type3.sta, type3.ades_utc_offset)
    )  %>%
    dplyr::mutate(diff_days =  (as.double(get_utc_time(type3.period_to) - get_utc_time(type3.period_from)))  ) %>%
    dplyr::mutate(sta_utc = dplyr::if_else(type3.sta_utc <= type3.std_utc, type3.sta_utc+(24*60*60), type3.sta_utc),
           type3.period_from_utc = as.Date(type3.std_utc),
           type3.period_to_utc = as.Date(type3.std_utc) + diff_days
    ) -> ssimjoin


  # Expand to flights
  if (pexpand)
  {
    ssimjoin %>%  dplyr::group_by_all() %>%
      tidyr::expand(n_flight = seq(1:(diff_days+1))) %>%
      dplyr::filter(diff_days > 0 | (diff_days == 0 & n_flight == 1) ) %>%
      dplyr::mutate(flight.flight_date = type3.std_utc + (n_flight-1)*24*60*60 ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(day_of_operation = as.character(get_day_of_week(as.Date(flight.flight_date)))) %>%
      dplyr::filter(stringr::str_detect(type3.days_of_operation, day_of_operation ) > 0) %>%
      dplyr::ungroup() %>%
      dplyr::select(-n_flight, day_of_operation) -> ssimjoin
  }


  # Filter selected columns
  collist[collist %in% colnames(ssimjoin)] -> collist
  ssimjoin %>% dplyr::select(-diff_days) %>%
    dplyr::select(collist) -> ssimjoin



  if (nestres) # use nested dataframes for type 3
  {
    ssimjoin %>%
      tidyr::nest(type3=all_of(collist[stringr::str_detect(collist,"type3.") | stringr::str_detect(collist,"flight.flight_date")])) -> ssimjoin2
    if (pexpand)
    {
      for (i in 1:nrow(ssimjoin2)){
        ssimjoin2[i,]$type3[[1]] <- ssimjoin2[i,]$type3[[1]] %>% tidyr::nest(flights=dplyr::all_of(collist[stringr::str_detect(collist,"flight.flight_date")]))
      }
    }
    ssimjoin <- ssimjoin2
  }

  if (cleannames)
  {
    ssimjoin %>% dplyr::select_all(list(~stringr::str_replace(.,"(type2.)|(type3.)|(type4.)",""))) -> ssimjoin
    if (nestres)
    {
      for (i in 1:nrow(ssimjoin)) {
        ssimjoin[i,]$type3[[1]] <- ssimjoin[i,]$type3[[1]] %>% dplyr::select_all(list(~stringr::str_replace(.,"(type2.)|(type3.)|(type4.)","")))
      }
    }
  }


  return(ssimjoin)
}

#' load_ssim_flights
#'
#' Load multiple SSIM file, instantiate flights, and return the result as a Data Frame. \cr
#' In case of period overlap for a specific flight date, information from the latest file will be used, \cr
#' so beware of the file order in
#' parameter ssim_files.
#'
#' @param ssim_files  List of SSIM files to load, in the correct order (from the first to load to the last file to load).
#' @param collist  List of columns that need to be present in the final Data Frame. get_ssim_collist() to get the full list.
#' @param cleannames  Clean column names in the final Data Frame by removing type2/type3 prefixes (TRUE/FALSE). Default TRUE.
#'
#' @return Data Frame containing the flights
#' @export
#'
#' @examples
#' # Display the total traffic per day from two SSIM schedules files
#' load_ssim(c("./AFR_20201115.txt", "./AFR_20201116.txt"), cleannames = FALSE) %>%
#' group_by(flight_date = as.Date(flight.flight_date)) %>%
#' summarise(total_flights = n()) %>%
#' arrange(desc(flight_date))
load_ssim_flights <- function(ssim_files = c("AFR_20201115.txt","AFR_20201116.txt"),
                              collist = get_ssim_collist(getall = FALSE),
                              cleannames=TRUE
)
{
  priotity <- length(ssim_files)
  all_flights <- data.frame()
  for (ssim in ssim_files)
  {
    all_flights <- base::rbind(all_flights,
                         load_ssim(ssim, nestres = FALSE,
                                   collist = get_ssim_collist(getall = TRUE),
                                   cleannames = FALSE,
                                   punpivot = FALSE,
                                   pexpand = TRUE) %>% dplyr::mutate(file_priority = priotity, flight_day = as.Date(flight.flight_date))
    )

    priotity <- priotity - 1
  }

  # Pick the latest file available for each flight day
  all_flights %>% dplyr::group_by(flight_day) %>%
    dplyr::summarise(choosen = min(file_priority))  -> choosen_flights

  all_flights %>%
    dplyr::inner_join(choosen_flights, by =  c("flight_day"="flight_day", "file_priority"="choosen")) -> all_flights

  # Filter selected columns
  collist[collist %in% colnames(all_flights)] -> collist
  all_flights %>% dplyr::select(-file_priority) %>%
    dplyr::select(collist) -> all_flights

  if (cleannames)
  {
    all_flights %>% dplyr::select_all(list(~stringr::str_replace(.,"(type2.)|(type3.)|(type4.)",""))) -> all_flights
  }
  return(all_flights)
}