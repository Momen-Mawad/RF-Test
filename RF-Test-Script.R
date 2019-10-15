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

  
# Plots -------------------------------------------------------------------

obs <- df %>% filter(satellite == "obs")
arc2 <- df %>% filter(satellite == "arc2")
chirps <- df %>% filter(satellite == "chirps")

#Histogram graphs

plot_histogram <- function(index) {
  ggplot(df) +
  geom_histogram(aes(x = index, fill = satellite)) +
    facet_wrap(~satellite, nrow = 2, ncol = 3)
    #xlab() ------------------
}

plot_histogram (df$`Max daily (mm)`)

plot_histogram (df$`RD>30`)

plot_histogram (df$`P99 daily (mm)`)

plot_histogram (df$`RD>P99`)


# Model quality table -----------------------------------------------------


