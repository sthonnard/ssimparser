testthat::test_that("CDG is LFPG", {
  testthat::expect_equal((ssimparser::load_ssim() %>% dplyr::filter(adep_iata == "CDG") %>% dplyr::select(adep_icao) %>% base::unique())[[1]], "LFPG")
  })
testthat::test_that("ALC is LEAL", {
  testthat::expect_equal((ssimparser::load_ssim() %>% dplyr::filter(ades_iata == "ALC") %>% dplyr::select(ades_icao) %>% base::unique())[[1]], "LEAL")
})

testthat::test_that("Expand two months",
{
   flights <- load_ssim_flights(c(ssimparser::get_ssim_sample(as.Date("2020-11-01"), as.Date("2020-11-30")),
                      ssimparser::get_ssim_sample(as.Date("2020-12-01"), as.Date("2020-12-31")))) %>%
                      dplyr::group_by(flight_date = as.Date(flight.flight_date)) %>%
                      dplyr::summarise(total_flights = dplyr::n()) %>%
                      dplyr::arrange(desc(flight_date))
   print(head(flights))
   testthat::expect_known_value(data.frame(flights), "two_months_flights")
})

testthat::test_that("Check all columns",
                    {
                      schedules <- ssimparser::load_ssim(ssimparser::get_ssim_sample(), collist = ssimparser::get_ssim_collist(getall = TRUE)[1:58], nested_df = FALSE)
                      testthat::expect_known_value(data.frame(schedules), "schedules")
                    })

testthat::test_that("Check all columns nested",
                    {
                      schedules <- ssimparser::load_ssim(ssimparser::get_ssim_sample(), collist = ssimparser::get_ssim_collist(getall = TRUE)[1:58], nested_df = TRUE)
                      testthat::expect_known_value(data.frame(schedules), "schedules_nested")
                    })

testthat::test_that("Check unpivot",
                    {
                      schedules <- ssimparser::load_ssim(ssimparser::get_ssim_sample(), collist = ssimparser::get_ssim_collist(getall = TRUE)[1:58], nested_df = FALSE,
                                                         unpivot_days_of_op = TRUE)
                      testthat::expect_known_value(data.frame(schedules), "unpivot")
                    })

testthat::test_that("Is aligned with python ssim package", {

  # Test ssimparser by comparing the result to Python package ssim
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")

  ssim_sample <- ssimparser::get_ssim_sample()
  # Write sample ssim file to temp dir
  write(ssim_sample,paste0(tempdir(),"/","ssim.txt"), append = FALSE)

  # Open the SSIM file with python package ssim
  reticulate::py_run_string(paste0("
import ssim
import pandas as pd
slots = ssim.read('",tempdir(),"/","ssim.txt","')
slots_df = pd.DataFrame(slots)
"))

  reticulate::py$slots_df %>% dplyr::filter(ad == "D" & record_type == 3) %>%
    dplyr::mutate(passenger_std = stringr::str_sub(scheduled_time_of_passenger_departure, 1, 4),
           std = stringr::str_sub(scheduled_time_of_aircraft_departure, 1, 4),
           adep_utc_offset = stringr::str_sub(scheduled_time_of_aircraft_departure, 5, 9),
           passenger_sta = stringr::str_sub(scheduled_time_of_passenger_arrival, 1, 4),
           sta = stringr::str_sub(scheduled_time_of_aircraft_arrival, 1, 4),
           ades_utc_offset = stringr::str_sub(scheduled_time_of_aircraft_arrival, 5, 9)) %>%
    dplyr::select(iata_airline = airline_designator, flight_number, service_type,
           period_from = period_of_operation_from,
           period_to = period_of_operation_to,
           days_of_operation,
           adep_iata = departure_station,
           ades_iata = arrival_station,
           aircraft_type_iata = aircraft_type,
           code_sharing = operating_airline_disclosure,
           passenger_std,
           std,
           adep_utc_offset,
           passenger_sta,
           sta,
           ades_utc_offset
    ) -> pyssim
  col_to_check <- colnames(pyssim)

  # Parse SSIM Schedule using this package
  ssim <- ssimparser::load_ssim(paste0(tempdir(),"/","ssim.txt"), expand_sched = FALSE, collist = ssimparser::get_ssim_collist(TRUE)) %>%
    dplyr::select(all_of(col_to_check))
  # Compare
  print(ssim)
  print(pyssim)

  # Compare result
  testthat::expect_true(setequal(ssim, pyssim))

})


