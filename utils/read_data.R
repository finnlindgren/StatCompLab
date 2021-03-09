# install.packages("LaF")

library(LaF)
library(tidyverse)
library(lubridate)
library(ggplot2)


read_ghcnd_values = function(station, path = '/home/flindgre/git/StatComp/GHCND')
{
  filename <- file.path(path, "ghcnd_all", paste0(station, ".dly"))
  file <- (LaF::laf_open_fwf(filename,
                             column_names=c(
                               "ID",
                               "Year","Month",
                               "Element",
                               paste(rep(1:31, each=4),
                                     rep(c("_Val", "_DMF","_QCF","_DSF"),
                                         times=31),
                                     sep="")),
                             column_widths=c(
                               11,
                               4,2,
                               4,
                               rep(c(5, 1, 1, 1),
                                   times=31)
                             ),
                             column_types=c(
                               "string",
                               "integer","integer","string",
                               rep(c("integer", "string","string","string"),
                                   times=31)),
                             trim=TRUE
  ))
  data <- next_block(file, nrow=LaF::nrow(file))
  LaF::close(file)

  data %>%
    gather("XYZZY", "XYZZYValue", -ID, -Year, -Month, -Element) %>%
    extract("XYZZY", c("Day", "Flag"), "X(..?)*_(...)") %>%
    mutate(Day = as.integer(Day)) %>%
    group_by_at(vars(-XYZZYValue)) %>%
    mutate(row_id=1:n()) %>% ungroup() %>%
    spread(key = Flag, value = XYZZYValue) %>%
    select(-row_id) %>%
    mutate(DMF = if_else(DMF == "", NA_character_, DMF),
           DSF = if_else(DSF == "", NA_character_, DSF),
           QCF = if_else(QCF == "", NA_character_, QCF),
           Value = if_else(Element %in% c("TMAX", "TMIN", "TAVG",
                                          "PRCP", "AWND", "EVAP",
                                          "MDEV", "MDPR",
                                          #"MDSV"???,
                                          "MDTN", "MDTX",
                                          "MNPN", "MXPN",
                                          "THIC",
                                          "TOBS",
                                          "ESD", "WESF",
                                          "WSF1", "WSF2", "WSF5",
                                          "WSFG", "WSFI", "WSFM"),
                           if_else(Val == "-9999", NA_real_, as.numeric(Val) / 10),
                           if_else(Val == "-9999", NA_real_, as.numeric(Val)))) %>%
    select(-Val) %>%
    filter(!is.na(Value)) %>%
    mutate(DecYear = decimal_date(ymd(paste(Year, Month, Day, sep = "-"))))
}


read_ghcnd_stations = function(filename = "ghcnd-stations.txt",
                               path = '/home/flindgre/git/StatComp/GHCND')
{
  filename <- file.path(path, filename)
  file <- (LaF::laf_open_fwf(filename,
                             column_names=c(
                               "ID",
                               "Latitude","Longitude",
                               "Elevation",
                               "State",
                               "Name",
                               "GSN_flag",
                               "HCN_CRN_flag",
                               "WMO_ID"),
                             column_widths=c(
                               11,
                               1+8,1+9,
                               1+6,
                               1+2,
                               1+30,
                               1+3,
                               1+3,
                               1+5),
                             column_types=c(
                               "string",
                               "numeric","numeric","numeric",
                               "string", "string","string","string", "integer"),
                             trim=TRUE
  ))
  data <- next_block(file, nrow=LaF::nrow(file))
  LaF::close(file)

  data %>%
    mutate(Elevation = if_else(as.character(Elevation) == "-999.9",
                               NA_real_, Elevation))
}

location_distances <- function(data, reference) {
  hav <- function(theta) { sin(theta/2)^2 }
  ihav <- function(theta) { 2 * asin(sqrt(abs(theta))) }
  ihav(
    hav((data$Latitude - reference$Latitude) * pi / 180) +
      cos(data$Latitude * pi / 180) * cos(reference$Latitude * pi / 180) *
      hav((data$Longitude - reference$Longitude) * pi / 180))
}

find_station_subset <- function(stations, ref = c(-4.1, 56.5), radius = 100) {
  earth_radius <- 6371
  Distance <- location_distances(stations,
                                 data.frame(Longitude = ref[1],
                                            Latitude = ref[2])) * earth_radius
  (stations %>% filter(Distance <= radius))$ID
}

filter_station_subset <- function(stations, ref = c(-4.1, 56.5), radius = 100) {
  stations %>% filter(ID %in% find_station_subset(stations, ref, radius))
}

read_data_subset <- function(stations) {
  do.call(rbind,
          lapply(stations$ID,
                 function(x) read_ghcnd_values(x)))
}


rearrange_data_1 <- function(data, stations, elements) {
  data %>%
    filter(Element %in% elements) %>%
    left_join(stations) %>%
    select(-DMF, -DSF, -QCF, -GSN_flag, -HCN_CRN_flag, -WMO_ID, -State)
}

rearrange_data_2 <- function(data, stations, element) {
  data %>%
    filter(Element == element) %>%
    spread(ID, Value) %>%
    select(-DMF, -DSF, -QCF)
}

rearrange_data_0 <- function(data, stations, elements) {
  data <- rearrange_data_1(data, stations, elements)
  data %>%
    pivot_wider(names_from = Element, values_from = Value)
}

