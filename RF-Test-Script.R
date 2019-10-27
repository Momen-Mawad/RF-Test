library(tidyverse)
library(hydroGOF)
library(shiny)

# Import satellite --------------------------------------------

all_satellite <- c("obs", "arc2", "chirps", "mswep", "persiann", "tamsat")

import_file <- function(folder) {
  files <- list.files(folder, pattern="*.csv", full.names=TRUE)
  map(files, read_csv) %>% set_names(files) %>% bind_rows(.id = "file")
  
}

df <- import_file(all_satellite) %>%
  separate(file, c("satellite","station1"), sep = "/") %>%
  separate(col = station1, into = c("station", NA), sep = "\\.")


# Important variables -----------------------------------------------------

obs <- df %>% filter(satellite == "obs")
arc2 <- df %>% filter(satellite == "arc2")
chirps <- df %>% filter(satellite == "chirps")
mswep <- df %>% filter(satellite == "mswep")
persiann <- df %>% filter(satellite == "persiann")
tamsat <- df %>% filter(satellite == "tamsat")


rainfall_indices <- c("Max daily (mm)", "RD>30", "P99 daily (mm)")

performance_matrix <- c("nse", "rmse", "mae", "mbe", "rsquare", "y_intercept", "slope")

all_stations <- df %>% distinct(station) %>% pull(station)

climate_zone <- read_csv("Climatic Zones.csv") %>% 
  arrange(stations) %>% mutate(all_stations) %>% 
  select(all_stations, climate_zone = `Climate Zone`)

all_zones <- climate_zone %>% distinct(climate_zone) %>% pull(climate_zone)

# General plots -----------------------------------------------------------

df_compare <- bind_rows(arc2, chirps, mswep, persiann, tamsat) %>% 
  bind_cols(as_tibble(rbind(obs, obs[1:1024, ], obs[1:1024, ], obs[1:1024, ],
                            obs[1:1024, ])))

df_hist_ploting <- function(m) {
  ggplot(df_compare) +
    geom_histogram(aes_string(x = paste("`", m, "`", sep = "")
                              , fill = "satellite")) +
    geom_histogram(aes_string(x = paste("`", m, 1, "`", sep = "")),
                   fill = "blue", alpha = 0.2) +
    facet_wrap(~satellite)
}

df_hist_ploting(rainfall_indices[1])
df_hist_ploting(rainfall_indices[2])
df_hist_ploting(rainfall_indices[3])


# Correlation: climatic zone vs rainfall index ----------------------------

df_compare <- df_compare %>% 
  left_join(climate_zone, by = c("station" = "all_stations"))

df_corr_ploting <- function(k) {
  ggplot(df_compare) +
    geom_smooth(aes_string(x = paste("`", k, "`", sep = ""),
                           y = paste("`", k, 1, "`", sep = "")),
                na.rm = T, method = lm, se = F) +
    facet_grid(satellite~climate_zone) +
    labs(x = "Ground Stations", y = "Satellite Products", 
         title = paste("Correlation between STEs and ground stations for", k))
  }

max(df_compare$"`Max daily (mm)`")

df_corr_ploting(rainfall_indices[1])
df_corr_ploting(rainfall_indices[2])
df_corr_ploting(rainfall_indices[3])
# Metrics table -----------------------------------------------------

evaluate_satellite <- function(satellite_name) {
  evaluate_index <- function(index_name) {
    evaluate_station <- function(station_name) {
      modely <- df %>% filter(satellite == satellite_name & station == station_name) %>%
        pull(index_name)
      
      modelx <- df %>% filter(satellite == "obs" & station == station_name) %>%
        pull(index_name)
      
      nse <- NSE(modely, modelx)
      
      rmse <- rmse(modely, modelx)
      
      mae <- mae(modely, modelx)
      
      mbe <- as.double(tdr::tdStats(modely, modelx, c("mbe")))
      
      rsquare <- as.double(tdr::tdStats(modely, modelx, c("r2")))
      
      y_intercept <- as.double(coefficients(lm(modely ~ modelx))[1])
      
      slope <- as.double(coefficients(lm(modely ~ modelx))[2])
      
      c(nse = nse, rmse = rmse, mae = mae, mbe = mbe, rsquare = rsquare, y_intercept = y_intercept, slope = slope)
    }
    
    metrics_stations <- map(all_stations, evaluate_station) %>% 
      set_names(all_stations) %>% bind_rows() %>% 
      mutate(performance_matrix) %>% 
      select(performance_matrix, everything())
  }
  
  metrics_indices <- map(rainfall_indices, evaluate_index) %>% 
    set_names(rainfall_indices) %>% bind_rows(.id = "metrics_indices")
  
}

metrics <- map(all_satellite[2:6], evaluate_satellite) %>% 
  set_names(all_satellite[2:6]) %>% bind_rows(.id = "metrics") %>% 
  pivot_longer(`Abu Hamed`:`Wau`, names_to =  "station") %>% 
  select(metrics_indices, performance_matrix, station, everything()) %>% 
  arrange(metrics_indices, performance_matrix, station)

# Ranking -----------------------------------------------------------------

ranking <- function(r) {
  metrics$value %>% matrix(nrow = 5) %>% .[ ,r] %>% rank()
  }

ranks <- map(1:672,ranking) %>% reshape::melt() %>% select(value) %>% set_names("ranks")

metrics <- metrics %>% bind_cols(ranks)

# Scores per performance------------------------------------------------------------------

metrics <- metrics %>% select(metrics_indices, station, metrics, everything()) %>% 
  arrange(metrics_indices, station, metrics)

scores_perf_func <- function(s){
  metrics$ranks %>% matrix(nrow = 7) %>% .[ ,s] %>% sum()
}

scores_perf <- tibble(rainfall_index = rep(rainfall_indices,each = 160),
                     stations = rep(all_stations,3 , each = 5),
                     satellite = rep(all_satellite[2:6], 96)) %>% 
  mutate(map_dbl(1:480, scores_perf_func)) %>% 
  rename(scores_performance = `map_dbl(1:480, scores_perf_func)`)


# Scores per rainfall index -----------------------------------------------

scores_perf <- scores_perf %>% 
  select(satellite, stations, everything()) %>% 
  arrange(satellite, stations)

scores_index_func <- function(i){
  scores_perf$scores_performance %>% matrix(nrow = 3) %>% .[ ,i] %>% sum()
}

scores_index <- tibble(satellite = rep(all_satellite[2:6], each = 32),
                 stations = rep(all_stations,5)) %>%  
  mutate(map_dbl(1:160, scores_index_func)) %>% 
  rename(scores_performance = `map_dbl(1:160, scores_index_func)`) %>% 
  select(stations, everything()) %>% 
  arrange(stations)

# Scores per stations -----------------------------------------------------------

ranking_station <- function(f) {
  scores_index$scores_performance %>% matrix(nrow = 5) %>% .[ ,f] %>%
    rank(ties.method = c("min"))
}

scores_station <- map(1:32,ranking_station) %>% reshape::melt() %>% select(value) %>%
  set_names("scores_station") %>% bind_cols(scores_index) %>% 
  select(stations, satellite, scores_station)

# Scores per climatic zones -----------------------------------------------

scores_station <- scores_station %>% 
  left_join(climate_zone, by = c("stations" = "all_stations")) %>% 
  select(climate_zone, satellite, everything()) %>% 
  arrange(climate_zone, satellite)

scoring_climate <- function(c){
  scoring_climate_satellite <- function(s) {
    scores_station %>% filter(climate_zone == c & satellite == s) %>% 
      .[4] %>% sum()
  }
  scores_arid <- map_dbl(all_satellite[2:6], scoring_climate_satellite) %>%
    set_names(all_satellite[2:6])
}

scores_climate_1 <- map(all_zones, scoring_climate) %>% set_names(all_zones) %>% 
  bind_rows() %>% mutate(satellite = all_satellite[2:6])

scores_climate_2 <-  map(all_zones, function(z){
                     rank(as.matrix(scores_climate_1 %>% select(z)), ties.method = "min")
                     }) %>% set_names(all_zones) %>% bind_rows() %>%
  mutate(`satellite` = all_satellite[2:6]) %>% 
  gather(`Hyper-Arid`:`Dry Sub-Humid`, key = `climatic zone`, value = `rank`)

# Plots -------------------------------------------------------------------
ggplot(scores_climate_2) +
  geom_point(aes(x = satellite, y = rank, color = `climatic zone`, shape = `climatic zone`),
             size = 3)

             
