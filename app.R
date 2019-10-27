library(shiny)
library(tidyverse)

ui <- fluidPage(    
  
  titlePanel("General overview for the dataset"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Plot Type:",
                  choices=c("Correlations", "Histograms")),
    
      selectInput("rain_index", "Rainfall Index:",
                  choices=c("Max daily (mm)", "RD>30", "P99 daily (mm)"),
                  selected = "Max daily (mm)"),
                  hr(),
                  helpText("Please choose plot type and spedify which rainfall index to be plotted"),
                  br(),
                  helpText("Developed by Momen M, Mawad"),
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Important variables -----------------------------------------------------

all_satellite <- c("obs", "arc2", "chirps", "mswep", "persiann", "tamsat")

import_file <- function(folder) {
  files <- list.files(folder, pattern="*.csv", full.names=TRUE)
  map(files, read_csv) %>% set_names(files) %>% bind_rows(.id = "file")
  
}

df <- import_file(all_satellite) %>%
  separate(file, c("satellite","station1"), sep = "/") %>%
  separate(col = station1, into = c("station", NA), sep = "\\.")

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

df_compare <- bind_rows(arc2, chirps, mswep, persiann, tamsat) %>% 
  bind_cols(as_tibble(rbind(obs, obs[1:1024, ], obs[1:1024, ], obs[1:1024, ],
                            obs[1:1024, ]))) %>%
  left_join(climate_zone, by = c("station" = "all_stations"))

# Sever -------------------------------------------------------------------

df_compare_app <<- df_compare 

server <- function(input, output) {
    output$plot <- renderPlot({
      if(input$plot_type == "Histograms") {
        ggplot2::ggplot(df_compare_app) +
          ggplot2::geom_histogram(aes_string(x = paste("`", input$rain_index, "`", sep = "")
                                             , fill = "satellite")) +
          ggplot2::geom_histogram(aes_string(x =paste("`", input$rain_index, 1, "`", sep = "")),
                                  fill = "blue", alpha = 0.2) +
          ggplot2::facet_wrap(~satellite)
      }
      else {
        ggplot(df_compare_app) +
          geom_smooth(aes_string(x = paste("`", input$rain_index, "`", sep = ""),
                                 y = paste("`", input$rain_index, 1, "`", sep = "")),
                      na.rm = T, method = lm, se = F) +
          facet_grid(satellite~climate_zone) +
          labs(x = "Ground Stations", y = "Satellite Products", 
               title = paste("Correlation between ESPs and ground stations for",
                             input$rain_index))
      }
    })
}



shinyApp(ui, server)
