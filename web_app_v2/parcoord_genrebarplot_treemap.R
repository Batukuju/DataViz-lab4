library(dplyr)
library(plotly)
library(ggplot2)
library(tidyr)

#data preprocessing
data_raw <- data.frame(read.csv("songs_normalize.csv"))
data <- data_raw %>%
  separate(col = genre, into = paste("col", 1:4), sep = ", ", ,fill = "right", extra = "drop") %>%
  gather(`genre`, key = "col", 18:21, na.rm = TRUE) %>%
  distinct(.keep_all = TRUE) %>%
  select(-col) %>%
  mutate(song = paste(song, artist, year, popularity, sep = "__"))

other<-function(quantity, type){
  if (quantity<10)
    return('other')
  else
    return(type)
}
### parcoord: atributes on x-axis
selected_attributes<-c("duration_ms", "danceability", "energy", "loudness", "speechiness", "genre")
selected_data<-data %>% 
  dplyr::select(selected_attributes)%>%
  group_by(genre)%>%
  summarise_all(funs(mean))

MASS::parcoord(selected_data[,2:length(selected_attributes)], var.label = TRUE, )
library(GGally)

p <- ggparcoord(selected_data,columns = 2:length(selected_attributes), groupColumn = 1)+
  theme_minimal()+
  theme(
    plot.title = element_text(size=10)
  )
p
ggplotly(p)

#### bar plot
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


###buuble graph
library(ggraph)
library(igraph)
library(tidyverse)
library(viridis)

bubble_attributes<-c("popularity", "genre", "song")

data_bubble<-data %>%
  dplyr::select(bubble_attributes)

# We need a data frame giving a hierarchical structure. Let's consider the flare dataset:
#genre->song
edges_genre<-data_bubble %>% transmute( from = 'origin', to = paste('origin',genre, sep="."))
edges_song<-data_bubble%>% transmute( from = paste('origin',genre, sep="."), to = paste('origin',genre, song, sep = "."))
edges<- unique(bind_rows(edges_song, edges_genre))

#vertices_genre<-unique(data_bubble  %>% group_by(genre) %>% summarise(size=sum(popularity)) %>% mutate(genre=paste('origin', genre, sep='.')))
vertices_genre<-unique(data_bubble  %>% group_by(genre) %>% summarise(size=0) %>% mutate(genre=paste('origin', genre, sep='.')))

vertices_song<-unique(transmute(data_bubble,genre=paste('origin',genre, song, sep = "."), size=popularity))
#vertices_genre<-rbind(vertices_genre,data.frame(genre='origin', size = as.integer(sum(data_bubble$popularity))))
vertices_genre<-rbind(vertices_genre,data.frame(genre='origin', size = as.integer(0)))

vertices<-unique(rbind(vertices_song,vertices_genre))

setdiff(edges$to, vertices$name)
setdiff(vertices$name,edges$from)

colnames(vertices)<-( c("name", "size"))
vertices$name<-reorder(vertices$name, vertices$size)


mygraph <- graph_from_data_frame( edges, vertices = vertices)

ggraph(mygraph, layout = 'circlepack', weight=size+1) + 
  geom_node_circle(aes(fill=as.factor(size+1))) +
  theme_void() + 
  theme(legend.position="FALSE") 
  

#nie działa
library(plotly)
edges_genre<-data_bubble %>% transmute( from = 'origin', to = paste('origin',genre, sep="."))
edges_song<-data_bubble%>% transmute( from = paste('origin',genre, sep="."), to = paste('origin',genre, song, sep = "."))
edges<- unique(bind_rows(edges_song, edges_genre))
edges<-rbind(edges,c('',"origin"))

plot_ly(  type="treemap",
  labels=as.vector(edges$to),
  parents=as.vector(edges$from))

###
library(plotly)
library(treemapify)
library(ggplot2)
data_t<- data%>%
  select('genre', 'popularity', 'song', 'year')

p<-ggplot(data_t,aes(area=popularity, fill=popularity, label=song, subgroup = genre, subgruoup2=year ))+
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                               "black",  min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T)
p
ggplotly(p)

