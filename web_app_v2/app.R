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
library(flexdashboard)
library(shinythemes)
library(GGally) #parcoord plot
library(plotly) #main plot
 


#magic function that allowes to some extend use valueBox from shinydashboard
#https://www.r-bloggers.com/2018/06/valuebox-without-shinydashboard-2/
valueBox <- function(value, subtitle, icon, color) {
  div(class = "col-4 col-2",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-3x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 28px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}

#main data
raw_data <- data.frame(read.csv("songs_normalize.csv")) %>%
  separate(col = genre, into = paste("col", 1:4), sep = ", ", ,fill = "right", extra = "drop") %>%
  gather(`genre_temp`, key = "col", 18:21, na.rm = TRUE) %>%
  distinct(.keep_all = TRUE) %>%
  mutate(`genre` = ifelse(`genre_temp` == "set()", "other", `genre_temp`)) %>%
  mutate(`duration_ms` = signif(`duration_ms` / 60000, 2)) %>%
  rename(`duration[min]` = `duration_ms`) %>%
  select(-col, -genre_temp)

data <- data_raw %>%
  separate(col = genre, into = paste("col", 1:4), sep = ", ", ,fill = "right", extra = "drop") %>%
  gather(`genre`, key = "col", 18:21, na.rm = TRUE) %>%
  distinct(.keep_all = TRUE) %>%
  select(-col) %>%
  mutate(song = paste(song, artist, year, popularity, sep = "__"))

#some additional helpful variables
summaryPopularity <- summary(raw_data$popularity)
summaryDanceability <- summary(raw_data$danceability)
summaryDuration <- summary(raw_data$`duration[min]`)
genres = unique(raw_data$genre)
# Define UI for application that draws a histogram
ui <- fluidPage(
  #different themes
  #theme = shinytheme("sandstone"),
  
  # Application title
  titlePanel("Top 2000 Spotify songs"),
  fluidPage(
  dashboardSidebar(
  fluidRow(
    #Main area for "future plots"
    (column(7,
           plotOutput("mainPlot"),
           tableOutput("table")
    )),
    column(5,
           mainPanel(
             tabsetPanel(
               #tab with summary plots
               tabPanel("Summary", 
                        "future_plot",
                        plotOutput("summaryPlot"),
                        plotOutput("summaryPlot2")
                        ), 
               #tab with sliders, gauge and valueBox
               tabPanel("Search", 
                        #popularity slider
                        sidebarPanel(width = 12,
                            sliderInput("popularitySlider",
                                        "Choose popularity:",
                                        min = 0,#summaryPopularity[[1]],
                                        max = 100,#summaryPopularity[[6]],
                                        value = c(50, 75)),
                            #duration slider
                            sliderInput("durationSlider",
                                        "Choose duration [minutes]:",
                                        min = 0,
                                        max = summaryDuration[[6]],
                                        value = c(2, 5)),
                            #danceability slider
                            sliderInput("danceabilitySlider",
                                        "Choose danceability:",
                                        min = 0,
                                        max = 100,
                                        value = c(25, 50)),
                            #selection of genres
                            selectInput("genreChoice",
                                        "Choose genres:",
                                        choices = genres,
                                        multiple = TRUE,
                                        selected = c("pop", "rock", "country")
                                        ),),
                        

                        #"crazy" valueBox
                        box(valueBox(value = "songsNumber",
                                 subtitle = "Number of songs",
                                 icon = "hashtag",
                                 color = "bg-info")),
                        #gauge
                        box(gaugeOutput("gauge"))
                        )
               ),
             )
           )
  
  ) 
)))

# Define server logic required to draw a histogram
server <- function(input, output) {
    #data from "Search" section, reevaluated for later use
    reactive_data <- reactive({
      filter(raw_data, 
             genre %in% input$genreChoice,
             popularity >= input$popularitySlider[1],
             popularity <= input$popularitySlider[2],
             danceability >= input$danceabilitySlider[1] / 100,
             danceability <= input$danceabilitySlider[2] / 100,
             `duration[min]` >= input$durationSlider[1],
             `duration[min]` <= input$durationSlider[2]) %>%
        select(artist, song, year, popularity, tempo,  danceability, `duration[min]`, genre) %>%
        arrange(-popularity)
    })
    
    ###MAIN PLOTS <- I couldn't make them interactive in no way. :/
    
    #I have no idea, why this doesn't work :/ 
    
    output$mainPlot <- renderPlot({
      bubble_attributes<-c("popularity", "genre", "song")
      data_bubble<-data %>%
        select(bubble_attributes)
      edges_genre<-data_bubble %>% transmute( from = 'origin', to = paste('origin',genre, sep="."))
      edges_song<-data_bubble%>% transmute( from = paste('origin',genre, sep="."), to = paste('origin',genre, song, sep = "."))
      edges<- unique(bind_rows(edges_song, edges_genre))
      edges<-rbind(edges,c('',"origin"))
      
      plot_ly(  type="treemap",
                labels=as.vector(edges$to),
                parents=as.vector(edges$from))
    })
    
    #parcoord plot summary
    output$summaryPlot <- renderPlot({
      
      selected_attributes<-c("duration_ms", "danceability", "energy", "loudness", "speechiness", "genre")
      selected_data<-data %>% 
        dplyr::select(selected_attributes)%>%
        group_by(genre)%>%
        summarise_all(funs(mean))
      p <- ggparcoord(selected_data,columns = 2:length(selected_attributes), groupColumn = 1)+
        theme_minimal()+
        theme(
          plot.title = element_text(size=10),
          legend.position = 'bottom'
        )
p
})
    other<-function(quantity, type){
      if (quantity<10)
        return('other')
      else
        return(type)
    }
    #barchart summary
    output$summaryPlot2 <- renderPlot({
      library(ggstream)
      library(ggplot2)
      
      
      geom_attributes<-c("year","popularity", "genre")
      
      data_geomstream<-data %>% 
        dplyr::select(geom_attributes) %>%
        filter(year>=2000)%>%
        group_by(year,genre) %>%
        count() %>%
        mutate(genre = other(n,genre))
      
      
      data_geomstream$genre<-reorder(data_geomstream$genre, data_geomstream$n)
      
      ggplot(data_geomstream, aes(fill=genre, y=n, x=year)) + 
        geom_bar(position='stack', stat='identity')
      
    })
    
    #Renders a table
    output$table <- renderTable(reactive_data())
    
    #renders number of songs
    output$songsNumber<- renderText({ 
      ifelse(is.null(length(reactive_data()[,1])), 0, length(reactive_data()[,1]))
    })
    
    #renders a gauge
    output$gauge = renderGauge({
      x <- reactive_data()$popularity
      gauge(round(ave(x)[1], 2),
            label = "Average popularity",
            min = 0, 
            max = 100#, 
            #sectors = gaugeSectors(success = c(0.5, 1), 
            #                       warning = c(0., 0.5),
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
