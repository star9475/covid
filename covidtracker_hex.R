setwd("~/Data Science")

#############hex chart
library(geojsonio)
library(rgeos)
library(broom)

spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")
# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
# I need to 'fortify' the data to be able to show it with ggplot2 (we need a data frame format)
spdf_fortified <- tidy(spdf, region = "google_name") %>%
  mutate(state = factor(state2abbr(id)))

# Merge geospatial and numerical information
spdf_fortified <- spdf_fortified %>%
  left_join(. , data_all) 
# # Make a first chloropleth map
# ggplot() +
#   geom_polygon(data = spdf_fortified, aes(fill =  positive, x = long, y = lat, group = group)) +
#   scale_fill_gradient(trans = "log") +
#   theme_void() +
#   coord_map()

data_us_last <- data_all %>% filter(date==max(dateChecked))

# spdf_fortified <- spdf_fortified %>%
#   left_join(. , data_all, by=c("id"="state")) 

ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill =positive, x = long, y = lat, group = group)) +
  scale_fill_gradient(low = "blue", high = "red", na.value = NA) +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_minimal() +
  coord_map() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())  +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(line = element_blank()) +
  labs(#x = "", y = "Importance", 
    title = "Confirmed COVID-19 cases per state",
    subtitle = "Data Updated: {frame_time}",
#    subtitle = paste0("Data Updated: ",format(max(date),'%b %d, %Y')),
    caption = "Data: https://covidtracking.com/ | Plot: @starkeeey") +
  transition_time(dateChecked)

get_hex_map <- function(d, scale = "log") {
  if (scale == "log") {
    
  d %>%
    filter(dateChecked==max(dateChecked)) %>%
    ggplot() +
    geom_polygon( aes(fill = log(positive+1), x = long, y = lat, group = group)) ->p
  }
  else {
    d %>%
      filter(dateChecked==max(dateChecked)) %>%
      ggplot() +
      geom_polygon( aes(fill = positive, x = long, y = lat, group = group)) ->p
  }
  
  p +
    scale_fill_gradient(low = "blue", high = "red", na.value = NA) +
    geom_text(data=centers, aes(x=x, y=y, label=id)) +
    theme_minimal() +
    coord_map() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())  +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    theme(line = element_blank()) +
    labs(#x = "", y = "Importance", 
      title = "Confirmed COVID-19 cases per state",
      #subtitle = "Data Updated: ",
      subtitle = paste0("Data Updated: " ,max(data$dateChecked)),
      caption = "Data: https://covidtracking.com/ | Plot: @starkeeey") 
  
}

get_hex_map_animated <- function(data) {
  data %>%
    #filter(dateChecked==max(dateChecked)) %>%
    ggplot() +
    geom_polygon( aes(fill =log(positive+1), x = long, y = lat, group = group)) +
    scale_fill_gradient(low = "blue", high = "red", na.value = NA) +
    geom_text(data=centers, aes(x=x, y=y, label=id)) +
    theme_minimal() +
    coord_map() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())  +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    theme(line = element_blank()) +
    labs(#x = "", y = "Importance", 
      title = "Confirmed COVID-19 cases per state",
      #subtitle = "Data Updated: ",
      subtitle = "Data Updated: {frame_time}",
      caption = "Data: https://covidtracking.com/ | Plot: @starkeeey") +
    transition_time(dateChecked)
  
}

