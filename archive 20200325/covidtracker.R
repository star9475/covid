library(tidyverse)
library(lubridate)
library(openintro) #for state abbr conversion
library(scales)
library(ggrepel)
library(viridis)
library(openintro) 

setwd("~/Data Science")

theme_josh <- function(base_size = 11,
                       base_family = "",
                       base_line_size = base_size / 170,
                       base_rect_size = base_size / 170){
  theme_bw(base_size = base_size, 
           base_family = base_family,
           base_line_size = base_line_size) %+replace%
    # theme_minimal(base_size = base_size, 
    #               base_family = base_family,
    #               base_line_size = base_line_size) %+replace%
    theme(
      plot.title = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255), 
        face = "bold",
        hjust = 0),
      plot.subtitle  = element_text(
        color = rgb(25, 43, 65, maxColorValue = 255), 
        #face = "bold",
        hjust = 0),
      axis.title = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.75)),
      axis.text = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.5)),
      legend.text = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.5)),
      legend.title = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.75)),
      
      strip.text = element_text(#face="bold", 
        size=8),
      strip.background = element_rect(fill="lightblue", color="black",size=1),
      
      plot.caption = element_text(
        color = rgb(105, 105, 105, maxColorValue = 255),
        size = rel(0.75), hjust = 1),
      # panel.grid.major = element_line(
      #   linetype = "dotted",
      #   rgb(105, 105, 105, maxColorValue = 255)),
      # panel.grid.minor = element_line(
      #   rgb(105, 105, 105, maxColorValue = 255),
      #   linetype = "dotted", 
      #   size = rel(4)),   
      
      complete = TRUE
    )
}

# states <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
#             "Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas",
#             "Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
#             "Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York",
#             "North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
#             "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia",
#             "Wisconsin","Wyoming", "District of Columbia")

## The first series of data have `Last Updated` in a different format.  Download separately
#dates <- c("01-22","01-23","01-24","01-25","01-26","01-27","01-28","01-29","01-30","01-31", "02-01")
# datalist = list()
data_pop <- usmap::statepop %>% 
#  rownames_to_column() %>% 
  rename(Population = pop_2015) %>%
  rename(state = abbr)

data <- read_csv(url("https://covidtracking.com/api/states/daily.csv"))

data_all <- data %>%
  mutate(date = ymd(date)) %>%
  mutate(label = if_else(date == max(date, na.rm=TRUE), state, NA_character_)) %>%
  inner_join(data_pop) %>%
  mutate(pos_test_per = positive / Population) %>%
  mutate(state = factor(state)) %>%
  mutate(date = if_else( (state == "WA"| state =="ID"), date - 1, date))
  
#mutate(label = if_else(positive == max(positive), state, NA_character_)) 

saveRDS(data_all, "data_covidtracking.rds")
#readRDS("data_covidtracking.rds")

#labs <- data_all %>% group_by(state) %>% summarise(tot = max(positive)) %>% arrange(desc(tot)) %>% head(4)
labs <- data_all %>% group_by(state) %>% summarise(tot = max(positive)) %>% arrange(desc(tot)) %>% head(4)
data_all <- data_all %>%
  mutate(islabel = if_else(state %in% labs, "YES", "NO"))


data_tohighlight <- data_all %>%
  filter(state %in% labs$state) %>%
  droplevels() #%>%
  #mutate(state = fct_relevel(state, positive))

labs2 <- data_all %>% 
  filter(dateChecked == max(dateChecked)) %>% 
  arrange(desc(positive)) %>%
  select(state, positive, date) %>%
  head(10)


data_remaining <- data_all %>%
  mutate(state = factor(state)) %>%
  filter(! state %in% labs$state)

#### US PLOT
data_tohighlight %>%
  mutate(state = fct_reorder(state, desc(positive))) %>%
  ggplot() +
  geom_line(aes(x=date, y=positive, group=state), data = data_remaining, colour = alpha("grey", 0.7)) +
  geom_label_repel(data = labs2,
                   aes(x=date, y=positive, label = state,group=state)) +
  geom_line(aes(x=date, y=positive, colour = state), size=1) +
  #scale_fill_viridis() +
  scale_color_manual(values = c("orange","red","orange","orange")) +
  scale_x_date(date_breaks = "days" , date_labels = "%b-%d") + scale_fill_viridis() +
  theme_minimal()+
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  theme(legend.position = "none")  +
  labs(x = "", y = "Positive Tests", 
       title = "Total Confirmed COVID-19 cases : United States",
       subtitle = paste0("Data Updated: ",max(data_all$dateChecked)),
       caption = "Data: https://covidtracking.com/ | Plot: @starkeeey") 


labs3 <- data_all %>% 
  filter(dateChecked == max(dateChecked)) %>% 
  arrange(desc(pos_test_per)) %>% 
  select(state,positive, pos_test_per, dateChecked)
  head(7) 

data_tohighlight <- data_all %>%
  filter(state %in% labs3$state) %>%
  droplevels()
    
#### US PLOT PER POP
data_tohighlight %>%
  mutate(state = fct_reorder(state, desc(pos_test_per))) %>%
  ggplot() +
  geom_line(aes(x=date, y=pos_test_per, group=state), data = data_remaining, colour = alpha("grey", 0.7)) +
  geom_label_repel(data = labs3,
                   aes(x=date, y=pos_test_per, label = state,group=state),
                   force = 1, point.padding = 0.1,nudge_x = 1,
                   segment.size = 0.2, size=3) +
  geom_line(aes(x=date, y=pos_test_per, colour = state), size=1) +
  #scale_fill_viridis() +
  #scale_color_manual(values = c("orange","red","orange","orange")) +
  scale_x_date(date_breaks = "days" , date_labels = "%b-%d") + scale_fill_viridis() +
  theme_minimal()+
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90),
        panel.grid.minor = element_blank(),
        legend.position = "none")  +
  labs(x = "", y = "", 
       title = "Total Confirmed COVID-19 cases Scaled: United States",
       subtitle = paste0("Data Updated: ",format(max(data_all$dateChecked),'%b %d, %Y')),
       caption = "Data: https://covidtracking.com/ | Plot: @starkeeey")




st <- "WA"
data_st <- data_all %>%
  filter(state == st) %>%
  arrange(date) %>%
  mutate( ppd= positive-lag(positive)) %>%
  mutate( npd = negative-lag(negative))
data_st[1,]$ppd = data_st[1,]$positive

### State Plot
ggplot(data = data_st, aes(x=date, y=ppd, group=1)) +  
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal()+
  geom_text(aes(label = ppd), hjust = .5, size = 2, color = "white",
            position = position_stack(vjust = .75)) +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90),
        panel.grid.minor = element_blank()) +
  #scale_y_continuous(limits = c(0,2000), breaks=seq(0,2000,200)) + 
  scale_x_date(date_breaks = "days" , date_labels = "%b-%d") +
  labs(#x = "", y = "Importance", 
    title = paste0("Confirmed COVID-19 cases per day : ", data_st[1,]$state),
    x = "", y = "",
    subtitle = paste0("Data Updated: ",format(max(data_st$date), '%b %d, %Y')),
    caption = "Data: https://covidtracking.com/ | Plot: @starkeeey")


#### plot of positive and negative tests
data_st %>% select(date,ppd,npd) %>%
  gather(test,results,2:3) %>%
  ggplot(aes(x=date,y=results, fill=test, position="dodge")) +
  geom_bar(stat = "identity", position="dodge") +
  scale_fill_manual(values = c( "#2c7bb6", "#d7181c")) +
  theme_minimal()+
  theme(legend.position="top",
        legend.title = element_blank()) +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90),
        panel.grid.minor = element_blank()) +
  scale_x_date(date_breaks = "days" , date_labels = "%b-%d") +
  labs(#x = "", y = "Importance", 
    title = paste0("COVID-19 Negative and Positive tests per day : ", data_st[1,]$state),
    x = "", y = "",
    subtitle = paste0("Data Updated: ",format(max(data_st$date), '%b %d, %Y')),
    caption = "Data: https://covidtracking.com/ | Plot: @starkeeey")  

ggplot(data = data_all, aes(x=date, y=positive, group=1)) +  
  #geom_path() +
  geom_point(size=.5) + geom_smooth(size=.5)+
  facet_wrap(~ state) +
  theme_josh() +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  labs(#x = "", y = "Importance", 
       title = "Confirmed COVID-19 cases per state",
       subtitle = paste0("Data Updated: ",max(data_all$dateChecked)),
       caption = "Data: https://covidtracking.com/ | Plot: @starkeeey")
  
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

# Calculate the centroid of each hexagon to add the label:
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()

# Merge geospatial and numerical information
spdf_fortified <- spdf_fortified %>%
  left_join(. , data_all) 

# Make a first chloropleth map
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill =  positive, x = long, y = lat, group = group)) +
  scale_fill_gradient(trans = "log") +
  theme_void() +
  coord_map()

data_us_last <- data_all %>% filter(date==max(date))

spdf_fortified <- spdf_fortified %>%
  left_join(. , data_all, by=c("id"="state")) 

ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill =positive, x = long, y = lat, group = group)) +
  scale_fill_gradient(low = "blue", high = "red", na.value = NA) +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_josh() +
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
  transition_time(date)
#transition_time(as.integer(date))

