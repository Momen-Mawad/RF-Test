library(tidyverse)

library(readxl)

#Importing observation data

OBS_list_names <- list.files("OBS/",pattern="*.csv",full.names=TRUE)

OBS_list <- lapply(OBS_list_names, read_csv)

OBS <- bind_rows(OBS_list) %>%
  select(1:11) %>%
  mutate(Station_Name=OBS_list_names)
  
view(OBS_list[[1]])
view(OBS_list[[2]])
#Importing simulated data from five satellites

ARC2_list = list.files("SIM/ARC2/",pattern="*.csv",full.names=TRUE)
ARC2 = lapply(ARC2_list, read_csv)

CHIRPS_list = list.files("SIM/CHIRPS/",pattern="*.csv",full.names=TRUE)
CHIRPS = lapply(CHIRPS_list, read_csv)

MSWEP_list = list.files("SIM/MSWEP/",pattern="*.csv",full.names=TRUE)
MSWEP = lapply(MSWEP_list, read_csv)

PERSIANN_list = list.files("SIM/PERSIANN/",pattern="*.csv",full.names=TRUE)
PERSIANN = lapply(PERSIANN_list, read_csv)

TAMSAT_list = list.files("SIM/TAMSAT/",pattern="*.csv",full.names=TRUE)
TAMSAT = lapply(TAMSAT_list, read_csv)
