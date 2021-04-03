# ssimparser

Parse IATA SSIM Schedules (Chapter 7) into a Data Frame.

*Note: currently limited to types 2 and 3.*

## Example of SSIM file

``` {style="width: 1600px;"}
1AIRLINE STANDARD SCHEDULE DATA SET     1                                                                                                                                                      000000001
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2LAF      W20 01NOV2001DEC2028NOV20SSIM EXAMPLE SCHEDULE        28NOV20CKENNY                               TEST AIRLINE                                                 1/8/13/18          ET1800000002
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3XAF 43310101J01NOV2001DEC2012345672CDG18451845+0100T1ALC20252020+01001F320XX                  XX   XX        XX XX    XXX      XX XX XX XX 1234   2L W                                         00000003
4XAF 43310101J              XX020CDGALCAF TEST                                                                                                                                                    000004
4XAF 43310101J              XX021CDGALCAF TEST                                                                                                                                                    000005
4XAF 43310101J              XX026CDGALCAF TEST                                                                                                                                                    000006
3XAF 43310101J01NOV2022NOV20     672CDG07000700+0100T1ALC08300830+01001F320XX                  XX   XX        XX XX    XXX      XX XX XX XX 1234   2L W                                         00000007
3XAF 12340101J01NOV2016NOV20     672CDG18451945+0100T1ALC21252120+01001F320XX                  XX   XX        XX XX    XXX      XX XX XX XX 1234   2L W                                         00000008
```

## Installation

To install the latest version from github:

``` {.R}
library(devtools)
install_github("sthonnard/ssimparser")

library(ssimparser)
```

## Examples

Display a sample SSIM file:

``` {.R}
ssimparser::get_ssim_sample()
```

Parse the sample file into a Data Frame:

``` {.R}
ssim_df <- ssimparser::load_ssim(ssim_file = get_ssim_sample())
print(ssim_df)
```

``` {style="width: 1700px;"}
# A tibble: 3 x 15
# Rowwise: 
  schedule_status iata_airline flight_number service_type period_from period_to days_of_operation adep_iata ades_iata aircraft_type_iata code_sharing std_utc             sta_utc             adep_icao ades_icao
  <chr>           <chr>        <chr>         <chr>        <chr>       <chr>     <chr>             <chr>     <chr>     <chr>              <chr>        <dttm>              <dttm>              <chr>     <chr>    
1 C               AF           4331          J            01NOV20     01DEC20   1234567           CDG       ALC       320                L            2020-11-01 17:45:00 2020-11-01 19:25:00 LFPG      LEAL     
2 C               AF           4331          J            01NOV20     22NOV20   67                CDG       ALC       320                L            2020-11-01 06:00:00 2020-11-01 07:30:00 LFPG      LEAL     
3 C               AF           1234          J            01NOV20     16NOV20   67                CDG       ALC       320                L            2020-11-01 18:45:00 2020-11-01 20:25:00 LFPG      LEAL     
```

Load any file from the filesystem:

``` {.R}
ssim_df <- ssimparser::load_ssim(ssim_file = "/tmp/ssim.txt")
```

Create one schedule per day of operation:

``` {.R}
ssim_df <- ssimparser::load_ssim(ssim_file = get_ssim_sample(), unpivot_days_of_op = TRUE)
print(ssim_df)
```

``` {style="width: 1700px;"}
# A tibble: 11 x 15
# Rowwise: 
   schedule_status iata_airline flight_number service_type period_from period_to days_of_operati… adep_iata ades_iata aircraft_type_i… code_sharing std_utc            
   <chr>           <chr>        <chr>         <chr>        <chr>       <chr>     <chr>            <chr>     <chr>     <chr>            <chr>        <dttm>             
 1 C               AF           1234          J            01NOV20     16NOV20   6                CDG       ALC       320              L            2020-11-01 18:45:00
 2 C               AF           1234          J            01NOV20     16NOV20   7                CDG       ALC       320              L            2020-11-01 18:45:00
 3 C               AF           4331          J            01NOV20     01DEC20   1                CDG       ALC       320              L            2020-11-01 17:45:00
 4 C               AF           4331          J            01NOV20     01DEC20   2                CDG       ALC       320              L            2020-11-01 17:45:00
 5 C               AF           4331          J            01NOV20     01DEC20   3                CDG       ALC       320              L            2020-11-01 17:45:00
 6 C               AF           4331          J            01NOV20     01DEC20   4                CDG       ALC       320              L            2020-11-01 17:45:00
 7 C               AF           4331          J            01NOV20     01DEC20   5                CDG       ALC       320              L            2020-11-01 17:45:00
 8 C               AF           4331          J            01NOV20     01DEC20   6                CDG       ALC       320              L            2020-11-01 17:45:00
 9 C               AF           4331          J            01NOV20     01DEC20   7                CDG       ALC       320              L            2020-11-01 17:45:00
10 C               AF           4331          J            01NOV20     22NOV20   6                CDG       ALC       320              L            2020-11-01 06:00:00
11 C               AF           4331          J            01NOV20     22NOV20   7                CDG       ALC       320              L            2020-11-01 06:00:00
# … with 3 more variables: sta_utc <dttm>, adep_icao <chr>, ades_icao <chr>
```

Parse SSIM to a nested Data Frame, which could be useful for investigating in RStudio:

``` {.R}
ssim_df <- ssimparser::load_ssim(ssim_file = get_ssim_sample(), nested_df = TRUE)
print(ssim_df)
```

``` {style="width: 1700px;"}
# A tibble: 1 x 3
# Rowwise: 
  schedule_status iata_airline type3            
  <chr>           <chr>        <list>           
1 C               AF           <tibble [3 × 13]>
```

``` {.R}
print(ssim_df$type3)
```

``` {style="width: 1700px;"}
# A tibble: 3 x 13
# Rowwise: 
  flight_number service_type period_from period_to days_of_operati… adep_iata ades_iata aircraft_type_i… code_sharing std_utc             sta_utc             adep_icao
  <chr>         <chr>        <chr>       <chr>     <chr>            <chr>     <chr>     <chr>            <chr>        <dttm>              <dttm>              <chr>    
1 4331          J            01NOV20     01DEC20   1234567          CDG       ALC       320              L            2020-11-01 17:45:00 2020-11-01 19:25:00 LFPG     
2 4331          J            01NOV20     22NOV20   67               CDG       ALC       320              L            2020-11-01 06:00:00 2020-11-01 07:30:00 LFPG     
3 1234          J            01NOV20     16NOV20   67               CDG       ALC       320              L            2020-11-01 18:45:00 2020-11-01 20:25:00 LFPG     
# … with 1 more variable: ades_icao <chr>
```

Expand the schedules to flights and display the traffic by month and departure airport ICAO:

``` {.R}
library(dplyr)
 ssimparser::load_ssim(ssim_file = get_ssim_sample(), expand_sched = TRUE) %>%
   group_by(month = format(flight.flight_date,"%Y-%m"), adep_icao) %>%
   summarise(n = n())
```

``` {style="width: 1700px;"}
# A tibble: 2 x 3
# Groups:   month [2]
  month   adep_icao     n
  <chr>   <chr>     <int>
1 2020-11 LFPG         42
2 2020-12 LFPG          1
```

Expand schedules to flights for multiple SSIM files:

``` {.R}
library(dplyr)
ssimparser::load_ssim_flights(ssim_files = c(get_ssim_sample(datefrom = as.Date("2020-11-01"), dateto = as.Date("2020-12-01")),
                                             get_ssim_sample(datefrom = as.Date("2020-11-15"), dateto = as.Date("2020-12-01")),
                                             get_ssim_sample(datefrom = as.Date("2020-11-10"), dateto = as.Date("2020-12-20"))
                                             )) %>%
  group_by(month = format(flight.flight_date,"%Y-%m"), adep_icao) %>%
  summarise(n = n())
```

``` {style="width: 1700px;"}
# A tibble: 2 x 3
# Groups:   month [2]
  month   adep_icao     n
  <chr>   <chr>     <int>
1 2020-11 LFPG         46
2 2020-12 LFPG         20
```

Expand the schedules to flights and display a line graph showing the traffic per day:

```{r}
library(dplyr)
library(ggplot2)
ssimparser::load_ssim(ssim_file = get_ssim_sample(), expand_sched = TRUE) %>%
  group_by(flight_day = as.Date(format(flight.flight_date,"%Y-%m-%d"))) %>%
  summarise(flights = n()) %>%
  ggplot(aes(flight_day, flights)) + geom_line()
```

Display a map of the connections between airports:

```{r}
library(dplyr)
library(ggplot2)
library(airportr)

ssimparser::load_ssim(ssim_file = get_ssim_sample(), expand_sched = TRUE) %>%
  mutate(ap1 = min(adep_iata, ades_iata),
         ap2 = max(adep_iata, ades_iata)) %>%
  group_by(ap1, ap2) %>%
  summarise(flights = n()) %>%
  mutate(ap1_det = airportr::airport_detail(ap1),
         ap2_det = airportr::airport_detail(ap2),
         citypair = paste0(ap1, "-", ap2)) -> flights 

citypair_plot <- rbind(data.frame(citypair = flights$citypair, x = flights$ap1_det$Longitude, y = flights$ap1_det$Latitude, size = flights$flights, name = flights$ap1_det$ICAO),
                       data.frame(citypair = flights$citypair, x = flights$ap2_det$Longitude, y = flights$ap2_det$Latitude, size = flights$flights, name = flights$ap2_det$ICAO))
                  
worldmap <- rnaturalearth::ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')
map <- sf::st_crop(worldmap, xmin = min(citypair_plot$x) - 10,
                   xmax = max(citypair_plot$x) + 10,
                   ymin = min(citypair_plot$y)  - 5,
                   ymax = max(citypair_plot$y) + 5)

ggplot2::ggplot() + 
  ggplot2::geom_sf(data = map, color = "#8c8c5a", fill = "#d7d7c1") +
  geom_line(aes(x=citypair_plot$x, y=citypair_plot$y, group=citypair_plot$citypair), linetype = 1, size = 1, alpha = 0.5 ) +
  geom_point(aes(x=citypair_plot$x, y=citypair_plot$y), size = 2) +
  geom_label(aes(x=citypair_plot$x, y=citypair_plot$y, label = citypair_plot$name)) +
  ggplot2::theme_bw() +
  ggplot2::labs(title = "Flights") +
  ggplot2::xlab("Longitude") +
  ggplot2::ylab("Latitude")
```
