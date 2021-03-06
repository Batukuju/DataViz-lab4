library(ggstream)
library(ggplot2)
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

data <- raw_data %>%
  mutate(song_key = paste(song, artist, year, popularity, sep = "__"))

#some additional helpful variables
summaryPopularity <- summary(raw_data$popularity)
summaryDanceability <- summary(raw_data$danceability)
summaryDuration <- summary(raw_data$`duration[min]`)
genres = unique(raw_data$genre)

ui <- fluidPage(
  #different themes
  theme = shinytheme("sandstone"),
  
  #application title
  titlePanel("Analyzer of the best songs from Spotify"),
  
  
  navbarPage("",
               
    tabPanel("Summary",
        fluidRow(
          h2("Comparison of the entire dataset"),
          h5("Compare the volume of different genres in the dataset and see how the volume changes in the scope of past years"),
          #Main area for "future plots"
          column(8,
                 plotlyOutput("mainPlot"),
                 #tableOutput("table")
                 
          ),
          column(4,
                 plotlyOutput("summaryPlot2")
          )
        ),
        h5("Songs matching your preference (see \"Search\" tab)"),
        tableOutput("table2")
    ),
    tabPanel("Search",
      fluidRow(
        h2("Choose your preferences and compare your result"),
        h5("Choose values of attributes you want to display found songs. You can also compare characteristics of different genres"),
        column(4,
                #number of songs to display
                numericInput("numberOfSongsToDisplay",
                            "How many songs to display below:",
                            value = 20,
                            min = 0,
                            max = 4000
                            ),
                #popularity slider
                sliderInput("popularitySlider",
                            "Choose popularity:",
                            min = 0,
                            max = 100,
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
                            selected = c("pop", "rock")
                
               ),
               #"crazy" valueBox
               valueBox(value = "songsNumber",
                        subtitle = "Number of songs matching preference",
                        icon = "hashtag",
                        color = "bg-info")
               ),
        column(8,
               #plot parcord chart
               plotlyOutput("summaryPlot"),
               #gauge
               gaugeOutput("gauge")
               )
      ),
      h5("Songs matching your preference"),
      tableOutput("table")
    ) 
  ) 
)
#define server logic required to draw a histogram
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
    
    reactive_mainPlot <- reactive({
      bubble_attributes<-c("popularity", "genre", "song_key")
      data_bubble<-data %>%
        select(bubble_attributes)
      edges_genre<-data_bubble %>% transmute( from = 'origin', to = paste('origin',genre, sep="."))
      edges_song<-data_bubble%>% transmute( from = paste('origin',genre, sep="."), to = paste('origin',genre, song_key, sep = "."))
      edges<- unique(bind_rows(edges_song, edges_genre))
      edges<-rbind(edges,c('',"origin"))
      edges
    })
    
    reactive_parcoordPlot <- reactive({
      selected_attributes<-c("duration[min]", "danceability", "energy", "loudness", "speechiness", "genre")
      selected_data<-data %>% 
        dplyr::select(selected_attributes)%>%
        filter(genre %in% input$genreChoice) %>%
        group_by(genre)%>%
        summarise_all(funs(mean))
      selected_data
    })
    
    reactive_barPlot <- reactive({
      geom_attributes<-c("year","popularity", "genre")
      data_geomstream<-data %>% 
        dplyr::select(geom_attributes) %>%
        filter(year>=2000)%>%
        group_by(year,genre) %>%
        count() %>%
        mutate(genre = other(n,genre)) %>%
        rename(`number of songs` = n)
      data_geomstream$genre<-reorder(data_geomstream$genre, data_geomstream$`number of songs`)
      data_geomstream
    })

    output$mainPlot <- renderPlotly({
      plot_ly(  type="treemap",
                labels=as.vector(reactive_mainPlot()$to),
                parents=as.vector(reactive_mainPlot()$from)) %>%
        layout(title = 'Volume of different genres', titlefont=list(size=18))
    })
    
    #parcoord plot summary
    output$summaryPlot <- renderPlotly({
      
      selected_attributes<-c("duration[min]", "danceability", "energy", "loudness", "speechiness", "genre")
      p <- ggparcoord(reactive_parcoordPlot(),columns = 2:length(selected_attributes), groupColumn = 1)+
        theme_minimal()+
        theme(
          plot.title = element_text(size=18),
          legend.position = 'bottom'
        ) + labs(title = "Parcoord graph of genres")
      
      ggplotly(p, tooltip = c("genre", "variable", "value"))
    })
    #helper function for barchart
    other<-function(quantity, type){
      if (quantity<10)
        return('other')
      else
        return(type)
    }
    #barchart summary
    output$summaryPlot2 <- renderPlotly({
      p <- ggplot(reactive_barPlot(), aes(fill=genre, y=`number of songs`, x=year)) + 
        geom_bar(position='stack', stat='identity') +
        theme(
          plot.title = element_text(size=18)
        ) + labs(title = "Barplot of genres in respect of the time")
      ggplotly(p)
    })
    
    #renders a table
    output$table <- renderTable(head(reactive_data(), n = input$numberOfSongsToDisplay), striped = TRUE,  hover = TRUE, rownames = TRUE)
    output$table2 <- renderTable(head(reactive_data(), n = input$numberOfSongsToDisplay), striped = TRUE,  hover = TRUE, rownames = TRUE)
    
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
            max = 100
      )
    })
}

#run the application 
shinyApp(ui = ui, server = server)
