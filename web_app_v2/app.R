#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(tidyr)
library(shiny)
raw_data <- data.frame(read.csv("songs_normalize.csv")) %>%
  separate(col = genre, into = paste("col", 1:4), sep = ", ", ,fill = "right", extra = "drop") %>%
  gather(`genre_temp`, key = "col", 18:21, na.rm = TRUE) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(`genre` = ifelse(`genre_temp` == "set()", "other", `genre_temp`)) %>%
  mutate(`duration_ms` = signif(`duration_ms` / 60000, 2)) %>%
  rename(`duration[min]` = `duration_ms`) %>%
  select(-col, -genre_temp)

summaryPopularity <- summary(raw_data$popularity)
summaryDanceability <- summary(raw_data$danceability)
summaryDuration <- summary(raw_data$`duration[min]`)
genres = unique(raw_data$genre)
# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Top 2000 Spotify songs"),
  
  fluidRow(
    column(7,
           plotOutput("summaryBox")
    ),
    column(5,
           mainPanel(
             tabsetPanel(
               tabPanel("Summary", "future_plot"), 
               tabPanel("Search", 
                        sliderInput("popularitySlider",
                                    "Choose popularity:",
                                    min = 0,#summaryPopularity[[1]],
                                    max = 100,#summaryPopularity[[6]],
                                    value = c(50, 75)),
                        sliderInput("durationSlider",
                                    "Choose duration [minutes]:",
                                    min = 0,
                                    max = summaryDuration[[6]],
                                    value = c(2, 5)),
                        sliderInput("danceabilitySlider",
                                    "Choose danceability:",
                                    min = 0,
                                    max = 100,
                                    value = c(25, 50)),
                        
                        selectInput("genreChoice", "Genres", choices = genres,  multiple = TRUE, selected = c("pop", "rock", "country"))
               ),
               tabPanel("Out", "OUTPUT",
                        verbatimTextOutput("genreChoice")
               )
               ),
             )
           )
    
  ),
  #another row of the page
  fluidRow(
    column(7,
           tableOutput("table")
    )
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$summaryBox <- renderPlot({
      barplot(table(raw_data$genre))
    })
    
    output$genreChoice <- renderPrint(input$popularitySlider[1])
    
    output$table <- renderTable(filter(raw_data, 
                                       genre %in% input$genreChoice,
                                       popularity >= input$popularitySlider[1],
                                       popularity <= input$popularitySlider[2],
                                       danceability >= input$danceabilitySlider[1] / 100,
                                       danceability <= input$danceabilitySlider[2] / 100,
                                       `duration[min]` >= input$durationSlider[1],
                                       `duration[min]` <= input$durationSlider[2]) %>%
                                  select(artist, song, year, popularity, tempo,  danceability, `duration[min]`, genre) %>%
                                  arrange(-popularity))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
