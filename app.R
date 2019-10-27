library(shiny)
library(ggplot2)

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
                  helpText("Histograms reveal the distribution of three rainfall indices for all stations combined"),
                  br(),
                  helpText("Developed by Momen M, Mawad"),
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

df_compare <<- df_compare 

server <- function(input, output) {
    output$plot <- renderPlot({
      if(input$plot_type == "Histograms") {
        ggplot2::ggplot(df_compare) +
          ggplot2::geom_histogram(aes_string(x = paste("`", input$rain_index, "`", sep = "")
                                             , fill = "satellite")) +
          ggplot2::geom_histogram(aes_string(x =paste("`", input$rain_index, 1, "`", sep = "")),
                                  fill = "blue", alpha = 0.2) +
          ggplot2::facet_wrap(~satellite)
      }
      else {
        ggplot(df_compare) +
          geom_smooth(aes_string(x = paste("`", input$rain_index, "`", sep = ""),
                                 y = paste("`", input$rain_index, 1, "`", sep = "")),
                      na.rm = T, method = lm, se = F) +
          facet_grid(satellite~climate_zone) +
          labs(x = "Ground Stations", y = "Satellite Products", 
               title = paste("Correlation between STEs and ground stations for",
                             input$rain_index))
      }
    })
  

}

shinyApp(ui, server)
