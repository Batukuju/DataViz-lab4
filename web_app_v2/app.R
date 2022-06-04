#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
raw_data <- data.frame(read.csv("songs_normalize.csv")) %>%
  separate(col = genre, into = paste("col", 1:4), sep = ", ", ,fill = "right", extra = "drop") %>%
  gather(`genre_temp`, key = "col", 18:21, na.rm = TRUE) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(`genre` = ifelse(`genre_temp` == "set()", "other", `genre_temp`)) %>%
  select(-col, -genre_temp)

summaryPopularity <- summary(raw_data$popularity)
summaryDanceability <- summary(raw_data$danceability)
summaryDuration <- summary(raw_data$duration_ms)
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
                                    min = summaryPopularity[[1]],
                                    max = summaryPopularity[[6]],
                                    value = c(summaryPopularity[[1]],summaryPopularity[[6]])),
                        sliderInput("durationSlider",
                                    "Choose duration:",
                                    min = summaryDuration[[1]],
                                    max = summaryDuration[[6]],
                                    value = c(summaryDuration[[1]],summaryDuration[[6]])),
                        sliderInput("danceabilitySlider",
                                    "Choose danceability:",
                                    min = summaryDanceability[[1]],
                                    max = summaryDanceability[[6]],
                                    value = c(summaryDanceability[[1]],summaryDanceability[[6]])),
                        
                        selectInput("genreChoice", "Genres", choices = genres,  multiple = TRUE)
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
    
    output$table <- renderTable(raw_data)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
