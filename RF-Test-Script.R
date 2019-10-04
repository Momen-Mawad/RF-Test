library(tidyverse)

library(readxl)

#Importing observation data

OBS_list = list.files("OBS/",pattern="*.csv",full.names=TRUE)
OBS = lapply(OBS_list, read.csv)

#Importing simulated data from five satellites

ARC2_list = list.files("SIM/ARC2/",pattern="*.csv",full.names=TRUE)
ARC2 = lapply(OBS_list, read.csv)

CHIRPS_list = list.files("SIM/CHIRPS/",pattern="*.csv",full.names=TRUE)
CHIRPS = lapply(CHIRPS_list, read.csv)

MSWEP_list = list.files("SIM/MSWEP/",pattern="*.csv",full.names=TRUE)
MSWEP = lapply(MSWEP_list, read.csv)

PERSIANN_list = list.files("SIM/PERSIANN/",pattern="*.csv",full.names=TRUE)
PERSIANN = lapply(PERSIANN_list, read.csv)

TAMSAT_list = list.files("SIM/TAMSAT/",pattern="*.csv",full.names=TRUE)
TAMSAT = lapply(TAMSAT_list, read.csv)