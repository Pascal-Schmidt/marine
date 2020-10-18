library(tidyverse)
library(geosphere)

ships <- readr::read_csv("data/ships.csv") %>% 
  janitor::clean_names()

# get ships that only have one observation in the data set
ships %>% 
  dplyr::count(shipname) %>% 
  dplyr::filter(n == 1) %>% 
  dplyr::pull(shipname) -> excluded_from_distance

# get data set with ships that only have one observation 
# and specify distance traveled to 0
ships %>% 
  dplyr::filter(shipname %in% excluded_from_distance) %>% 
  dplyr::mutate(distance_traveled = 0) -> ships_excluded_append

# calculate distance between consecutive points
ships %>%
  dplyr::filter(!(shipname %in% excluded_from_distance)) %>% 
  dplyr::group_split(ship_id) %>% 
  purrr::map( ~
                dplyr::arrange(., datetime) %>% 
                dplyr::mutate(., distance_traveled = c(NA, geosphere::distHaversine(as.matrix(.[, c("lon", "lat")])))) 
  ) %>% 
  do.call(rbind, .) -> ships

ships %>% 
  dplyr::bind_rows(ships_excluded_append) %>% 
  dplyr::mutate(distance_traveled = round(distance_traveled, 2)) %>% 
  dplyr::select(lat, lon, destination, shipname,
                ship_id, datetime, port, ship_type,
                distance_traveled) -> ships

feather::write_feather(ships, "data/ships.feather")
