library(tidyverse)

#----------------------------------------------------------------Importing obs data

#Importing observation data as a list with names only
obs_list_names <- list.files("obs/",pattern="*.csv",full.names=TRUE)

#Getting Sattelite & Station as Variables in a tibble
satellite_station <- as.tibble(obs_list_names) %>%
  separate(value, c("satellite","station1"), sep = "/") %>%
  separate(col = station1, into = c("station", NA), sep = "\\.")

#Adding all obs data in one list
obs_list <- lapply(obs_list_names, read_csv)

#Giving names to obs_list according to each station
names(obs_list) <- satellite_station$Station

#Expanding satellite_station to mach obs number of observations (obs comes in the next step)
satellite_station_expanded <- tibble(
  satelite = "obs",
  station = rep(satellite_station$station, each = 32)
)

#Combining all dataframes in obs_list in one dataframe obs
obs <- bind_rows(obs_list) %>%
  bind_cols(satellite_station_expanded)

#----------------------------------------------------------------Importing arc2 data

ARC2_list = list.files("sim/ARC2/",pattern="*.csv",full.names=TRUE)
ARC2 = lapply(ARC2_list, read_csv)

CHIRPS_list = list.files("sim/CHIRPS/",pattern="*.csv",full.names=TRUE)
CHIRPS = lapply(CHIRPS_list, read_csv)

MSWEP_list = list.files("sim/MSWEP/",pattern="*.csv",full.names=TRUE)
MSWEP = lapply(MSWEP_list, read_csv)

PERSIANN_list = list.files("sim/PERSIANN/",pattern="*.csv",full.names=TRUE)
PERSIANN = lapply(PERSIANN_list, read_csv)

TAMSAT_list = list.files("sim/TAMSAT/",pattern="*.csv",full.names=TRUE)
TAMSAT = lapply(TAMSAT_list, read_csv)
