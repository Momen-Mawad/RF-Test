library(tidyverse)

#----------------------------------------------------------------Importing obs data

#Importing observation data as a list with names only
obs_list_names <- list.files("obs/",pattern="*.csv",full.names=TRUE)

#Getting satellite & station as Variables in a tibble
satellite_station <- as.tibble(obs_list_names) %>%
  separate(value, c("satellite","station1"), sep = "/") %>%
  separate(col = station1, into = c("station", NA), sep = "\\.")

#Adding all obs data in one list
obs_list <- lapply(obs_list_names, read_csv)

#Giving names to obs_list according to each station
names(obs_list) <- satellite_station$station

#Expanding satellite_station to mach obs number of observations (obs comes in the next step)
satellite_station_expanded <- tibble(
  satelite = "obs",
  station = rep(satellite_station$station, each = 32)
)

#Combining all dataframes in obs_list in one dataframe obs
obs <- bind_rows(obs_list) %>%
  bind_cols(satellite_station_expanded)

remove(satellite_station, satellite_station_expanded, obs_list_names, obs_list)

#----------------------------------------------------------------Importing arc2 data
#Importing arc2 data as a list with names only
arc2_list_names <- list.files("arc2/",pattern="*.csv",full.names=TRUE)

#Getting satellite & station as Variables in a tibble
satellite_station <- as.tibble(arc2_list_names) %>%
  separate(value, c("satellite","station1"), sep = "/") %>%
  separate(col = station1, into = c("station", NA), sep = "\\.")

#Adding all arc2 data in one list
arc2_list <- lapply(arc2_list_names, read_csv)

#Giving names to arc2_list according to each station
names(arc2_list) <- satellite_station$station

#Expanding satellite_station to mach arc2 number of observations (arc2 comes in the next step)
satellite_station_expanded <- tibble(
  satelite = "arc2",
  station = rep(satellite_station$station, each = 32)
)

#Combining all dataframes in arc2_list in one dataframe arc2
arc2 <- bind_rows(arc2_list) %>%
  bind_cols(satellite_station_expanded)

remove(satellite_station, satellite_station_expanded, arc2_list_names, arc2_list)

#----------------------------------------------------------------Importing chirps data
#Importing chirps data as a list with names only
chirps_list_names <- list.files("chirps/",pattern="*.csv",full.names=TRUE)

#Getting satellite & station as Variables in a tibble
satellite_station <- as.tibble(chirps_list_names) %>%
  separate(value, c("satellite","station1"), sep = "/") %>%
  separate(col = station1, into = c("station", NA), sep = "\\.")

#Adding all chirps data in one list
chirps_list <- lapply(chirps_list_names, read_csv)

#Giving names to chirps_list according to each station
names(chirps_list) <- satellite_station$station

#Expanding satellite_station to mach chirps number of observations (chirps comes in the next step)
satellite_station_expanded <- tibble(
  satelite = "chirps",
  station = rep(satellite_station$station, each = 32)
)

#Combining all dataframes in chirps_list in one dataframe chirps
chirps <- bind_rows(chirps_list) %>%
  bind_cols(satellite_station_expanded)

remove(satellite_station, satellite_station_expanded, chirps_list_names, chirps_list)

#----------------------------------------------------------------Importing mswep data
#Importing mswep data as a list with names only
mswep_list_names <- list.files("mswep/",pattern="*.csv",full.names=TRUE)

#Getting satellite & station as Variables in a tibble
satellite_station <- as.tibble(mswep_list_names) %>%
  separate(value, c("satellite","station1"), sep = "/") %>%
  separate(col = station1, into = c("station", NA), sep = "\\.")

#Adding all mswep data in one list
mswep_list <- lapply(mswep_list_names, read_csv)

#Giving names to mswep_list according to each station
names(mswep_list) <- satellite_station$station

#Expanding satellite_station to mach mswep number of observations (mswep comes in the next step)
satellite_station_expanded <- tibble(
  satelite = "mswep",
  station = rep(satellite_station$station, each = 32)
)

#Combining all dataframes in mswep_list in one dataframe mswep
mswep <- bind_rows(mswep_list) %>%
  bind_cols(satellite_station_expanded)

remove(satellite_station, satellite_station_expanded, mswep_list_names, mswep_list)

#----------------------------------------------------------------Importing persiann data
#Importing persiann data as a list with names only
persiann_list_names <- list.files("persiann/",pattern="*.csv",full.names=TRUE)

#Getting satellite & station as Variables in a tibble
satellite_station <- as.tibble(persiann_list_names) %>%
  separate(value, c("satellite","station1"), sep = "/") %>%
  separate(col = station1, into = c("station", NA), sep = "\\.")

#Adding all persiann data in one list
persiann_list <- lapply(persiann_list_names, read_csv)

#Giving names to persiann_list according to each station
names(persiann_list) <- satellite_station$station

#Expanding satellite_station to mach persiann number of observations (persiann comes in the next step)
satellite_station_expanded <- tibble(
  satelite = "persiann",
  station = rep(satellite_station$station, each = 32)
)

#Combining all dataframes in persiann_list in one dataframe mswep
persiann <- bind_rows(persiann_list) %>%
  bind_cols(satellite_station_expanded)

remove(satellite_station, satellite_station_expanded, persiann_list_names, persiann_list)

#----------------------------------------------------------------Importing tamsat data
#Importing tamsat data as a list with names only
tamsat_list_names <- list.files("tamsat/",pattern="*.csv",full.names=TRUE)

#Getting satellite & station as Variables in a tibble
satellite_station <- as.tibble(tamsat_list_names) %>%
  separate(value, c("satellite","station1"), sep = "/") %>%
  separate(col = station1, into = c("station", NA), sep = "\\.")

#Adding all tamsat data in one list
tamsat_list <- lapply(tamsat_list_names, read_csv)

#Giving names to tamsat_list according to each station
names(tamsat_list) <- satellite_station$station

#Expanding satellite_station to mach tamsat number of observations (tamsat comes in the next step)
satellite_station_expanded <- tibble(
  satelite = "tamsat",
  station = rep(satellite_station$station, each = 32)
)

#Combining all dataframes in persiann_list in one dataframe mswep
tamsat <- bind_rows(tamsat_list) %>%
  bind_cols(satellite_station_expanded)

remove(satellite_station, satellite_station_expanded, tamsat_list_names, tamsat_list)

#--------------------------------import climatic zones files
climatic_zones <- read_csv("Climatic Zones.csv")
climatic_zones
#-------------------------------------------------------------------Combine all in one tibble

df <- bind_rows(obs, arc2, chirps, mswep, persiann, tamsat) %>%

  left_join(climatic_zones) #There is a problem here

#---------------------------------------------------------------------ggplot2

