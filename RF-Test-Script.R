library(tidyverse)

# Import satellite --------------------------------------------

satellite <- c("obs", "arc2", "chirps", "mswep", "persiann", "tamsat")

import_file <- function(folder) {
  files <- list.files(folder, pattern="*.csv", full.names=TRUE)
  map(files, read_csv) %>% set_names(files) %>% bind_rows(.id = "file")
  
}

df <- import_file(satellite) %>%
  separate(file, c("satellite","station1"), sep = "/") %>%
  separate(col = station1, into = c("station", NA), sep = "\\.")

#---------------------------------------------------------------------ggplot2
#creating function for regression lines graph
plot_rl = function(x) {
ggplot(obs) +
  geom_smooth(aes(`Max daily (mm)`, x$`Max daily (mm)`), method = "lm") +
  xlab ("obs") +
  ylab("x") #????? 
}
plot_rl(arc2)
plot_rl(chirps)
plot_rl(mswep)
plot_rl(persiann)
plot_rl(tamsat)

#Histogram graphs

ggplot(dt, aes(x = `Max daily (mm)`)) +
  geom_histogram(aes(fill = satellite))  +
  #geom_histogram(data = filter(dt, satellite == "obs"), aes(x = `Max daily (mm)`)) +
  facet_wrap(~satellite, nrow = 2, ncol = 3)  

#------------------------------------------------------------------------------
