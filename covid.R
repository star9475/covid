library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(lubridate)
library(openintro) #for state abbr conversion
library(scales)
library(ggrepel)
library(openintro) 
library(geofacet)

#setwd("~/Data Science/covid")

RUNCHARTS = FALSE

##### Get the data
data_pop <- usmap::statepop %>% 
#  rownames_to_column() %>% 
  rename(Population = pop_2015) %>%
  rename(state = abbr)

data <- read_csv(url("https://covidtracking.com/api/states/daily.csv"))
saveRDS(data, "data_covidtracking.rds")
#data <- readRDS("data_covidtracking.rds")

data_all <- data %>%
  mutate(date = ymd(date)) %>%
  mutate(dateChecked = as_date(dateChecked)) %>%
  mutate(label = if_else(date == max(date, na.rm=TRUE), state, NA_character_)) %>%
  inner_join(data_pop) %>%
  mutate(pos_test_per = positive / Population) %>%
  mutate(state = factor(state)) %>%
  mutate(date = if_else( (state == "WA"| state =="ID"), date - 1, date)) %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate( ppd= positive-lag(positive)) %>%
  mutate( npd = negative-lag(negative)) %>%
  ungroup()

get_state_plot_ppd <- function(data,  st = "WA") {
  
  #st <- "WA"
  #data_st[1,]$ppd = data_st[1,]$positive
  data_st = data %>%
    filter(state == st)
  
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
  
}

get_state_plot_ppd_npd <- function(data, st = "WA") {
  data_st <- data %>%
    filter(state == st)
  
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
  
  
}

get_us_facet <- function(data) {
data %>%
ggplot( aes(x=date, y=pos_test_per, group=1)) +  
  #geom_path() +
  geom_path(size=1, color = "#2c7bb6") + #geom_smooth(size=.3)+
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 90)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+       
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.minor = element_blank())+
  facet_geo(~ state, grid = "us_state_grid2") +
  labs(#x = "", y = "Importance", 
    title = "Confirmed COVID-19 cases per state scaled for population",
    subtitle = paste0("Data Updated: ",max(data_all$dateChecked)),
    caption = "Data: https://covidtracking.com/ | Plot: @starkeeey") 
}

get_us_linechart_log <- function(d) {
  # data_tohighlight %>%
  #   mutate(state = fct_reorder(state, desc(positive))) %>%
  #   ggplot() +
  #   geom_line(aes(x=date, y=positive, group=state), data = data_remaining, colour = alpha("grey", 0.7)) +
  #   geom_label_repel(data = labs2,
  #                    aes(x=date, y=positive, label = state,group=state)) +
  #   geom_line(aes(x=date, y=positive, colour = state), size=1) 
  
  ### Redo US line plot
  data_to_highlight <- d %>% group_by(state) %>% summarise(tot = max(positive)) %>% arrange(desc(tot)) %>% head(8)
  
  data_highlighted <- d %>%
    group_by(state) %>%
    filter(positive > 0) %>%
    arrange(date) %>%
    mutate(ppd_na = na_if(ppd,0)) %>%
    mutate(days_elapsed = date-min(date),
           end_label = if_else(date == max(date), as.character(state), NA_character_) ) %>% 
    select(date, state, positive, ppd, days_elapsed, end_label) %>%
    filter(state %in% data_to_highlight$state) %>%
    droplevels() 
  
  data_gray <- d %>%
    group_by(state) %>%
    filter(positive > 0) %>%
    arrange(date) %>%
    mutate(ppd_na = na_if(ppd,0)) %>%
    mutate(days_elapsed = date-min(date)) %>% 
    select(date, state, positive, ppd, days_elapsed) %>%
    filter(! state %in% data_to_highlight$state) %>%
    droplevels()
  
  ggplot() +
    geom_line(data = data_highlighted, 
              aes(x=days_elapsed, y=log10(positive+1), 
                                           group = state, 
                                           color = state, 
                                           label=end_label),
              size = 0.75) + 
    geom_point(data = data_highlighted, 
               aes(x=days_elapsed, y=log10(positive+1), 
                   group = state,
                   color = state),
               size = 0.9) + 
    geom_line(data = data_gray, 
              aes(x=days_elapsed, y=log10(positive+1), group=state), 
              colour = alpha("grey", 0.7) ,
              size = 0.7 ) +
    #scale_x_continuous(limits = c(5, 25)) +
    geom_label_repel(data = data_highlighted,
                     aes(x=days_elapsed, y=log10(positive+1), color=state),
                     label = data_highlighted$end_label,
                     nudge_x = 1.1, #1
                     nudge_y = 0.1,
                     #segment.color = NA,
                     force = 1,
                     point.padding = 0.1,
                     segment.size = 0.2, size=3) +
    
    scale_color_brewer(palette="Dark2") +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Days since first reported positive test", 
         y = "Positive Tests Log10 Scale", 
         title = "Total Confirmed COVID-19 cases Log 10 Scaled: United States",
         subtitle = paste0("Data Updated: ",format(max(data_all$dateChecked),'%b %d, %Y')),
         caption = "Data: https://covidtracking.com/ | Plot: @starkeeey")
  
  #rm(data_highlighted, data_gray, data_to_highlight)
}
  
get_us_linechart <- function(d) {

  ### Redo US line plot
  data_to_highlight <- d %>% group_by(state) %>% summarise(tot = max(positive)) %>% arrange(desc(tot)) %>% head(8)
  
  data_highlighted <- d %>%
    group_by(state) %>%
    filter(positive > 0) %>%
    arrange(date) %>%
    mutate(ppd_na = na_if(ppd,0)) %>%
    mutate(days_elapsed = date-min(date),
           end_label = if_else(date == max(date), as.character(state), NA_character_) ) %>% 
    select(date, state, positive, ppd, days_elapsed, end_label) %>%
    filter(state %in% data_to_highlight$state) %>%
    droplevels() 
  
  data_gray <- d %>%
    group_by(state) %>%
    filter(positive > 0) %>%
    arrange(date) %>%
    mutate(ppd_na = na_if(ppd,0)) %>%
    mutate(days_elapsed = date-min(date)) %>% 
    select(date, state, positive, ppd, days_elapsed) %>%
    filter(! state %in% data_to_highlight$state) %>%
    droplevels()
  
  ggplot() +
    geom_line(data = data_highlighted, 
              aes(x=days_elapsed, y=positive, 
                  group = state, 
                  color = state, 
                  label=end_label),
              size = 1) #0.75) + 
    geom_point(data = data_highlighted, 
               aes(x=days_elapsed, y=positive, 
                   group = state,
                   color = state),
               size = 2) # 0.9) + 
    geom_line(data = data_gray, 
              aes(x=days_elapsed, y=positive+1, group=state), 
              colour = alpha("grey", 0.7) ,
              size = 0.7 ) +
    scale_x_continuous(limits = c(5, max(data_highlighted$days_elapsed))) +
    geom_label_repel(data = data_highlighted,
                     aes(x=days_elapsed, y=positive, color=state),
                     label = data_highlighted$end_label,
                     nudge_x = 1.1, #1
                     nudge_y = 0.1,
                     #segment.color = NA,
                     force = 1,
                     point.padding = 0.1,
                     segment.size = 0.2, size=3) +
    
    scale_color_brewer(palette="Dark2") +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Days since first reported positive test", 
         y = "Positive Tests", 
         title = "Total Confirmed COVID-19 cases: United States",
         subtitle = paste0("Data Updated: ",format(max(data_all$dateChecked),'%b %d, %Y')),
         caption = "Data: https://covidtracking.com/ | Plot: @starkeeey")
  
  #rm(data_highlighted, data_gray, data_to_highlight)
}
  
get_us_linechart_scaled <- function(d) {

  ### Redo US line plot
  data_to_highlight <- d %>% group_by(state) %>% summarise(tot = max(pos_test_per)) %>% arrange(desc(tot)) %>% head(8)
  
  data_highlighted <- d %>%
    group_by(state) %>%
    filter(positive > 0) %>%
    arrange(date) %>%
    mutate(ppd_na = na_if(ppd,0)) %>%
    mutate(days_elapsed = date-min(date),
           end_label = if_else(date == max(date), as.character(state), NA_character_) ) %>% 
    select(date, state, positive, ppd, days_elapsed, end_label, pos_test_per) %>%
    filter(state %in% data_to_highlight$state) %>%
    droplevels() 
  
  data_gray <- d %>%
    group_by(state) %>%
    filter(positive > 0) %>%
    arrange(date) %>%
    mutate(ppd_na = na_if(ppd,0)) %>%
    mutate(days_elapsed = date-min(date)) %>% 
    select(date, state, positive, ppd, days_elapsed, pos_test_per) %>%
    filter(! state %in% data_to_highlight$state) %>%
    droplevels()
  
  ggplot() +
    geom_line(data = data_highlighted, 
              aes(x=days_elapsed, y=pos_test_per, 
                  group = state, 
                  color = state, 
                  label=end_label),
              size =1) +# 0.75) + 
    geom_point(data = data_highlighted, 
               aes(x=days_elapsed, y=pos_test_per, 
                   group = state,
                   color = state),
               size = 2) +#0.9) + 
    geom_line(data = data_gray, 
              aes(x=days_elapsed, y=pos_test_per, group=state), 
              colour = alpha("grey", 0.7) ,
              size = 0.5 ) +
    scale_x_continuous(limits = c(10, max(data_highlighted$days_elapsed))) +
    geom_label_repel(data = data_highlighted,
                     aes(x=days_elapsed, y=pos_test_per, color=state),
                     label = data_highlighted$end_label,
                     #nudge_x = 1.1, #1
                     #nudge_y = 0.1,
                     #segment.color = NA,
                     force = 1,
                     point.padding = 0.1,
                     segment.size = 0.2, size=3) +
    
    scale_color_brewer(palette="Dark2") +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Days since first reported positive test", 
         y = "Positive Tests Per Population", 
         title = "Total Confirmed COVID-19 Scaled: United States",
         subtitle = paste0("Data Updated: ",format(max(data_all$dateChecked),'%b %d, %Y')),
         caption = "Data: https://covidtracking.com/ | Plot: @starkeeey")
  
  #rm(data_highlighted, data_gray, data_to_highlight)
}

if (RUNCHARTS) {
  
p1 <- get_state_plot_ppd(data_all)
p2 <- get_state_plot_ppd_npd(data_all)
p3 <- get_state_plot_ppd_npd(data_all, "ID")
p4 <- get_us_facet(data_all)
p5 <- get_us_linechart_log(data_all)
p6 <- get_us_linechart(data_all)
p7 <- get_us_linechart_scaled(data_all)

#p8 <- get_hex_map(spdf_fortified, scale = "normal")


ggsave("plots/wa_2020_03_26.png",
       p2, width = 6, height = 4)
ggsave("plots/id_2020_03_26.png",
       p3, width = 6, height = 4)
ggsave("plots/facet_2020_03_26.png",
       p4, width = 6, height = 4)
ggsave("plots/uslog_2020_03_26.png",
       p5, width = 6, height = 4)
ggsave("plots/usline_2020_03_26.png",
       p6, width = 6, height = 4)
ggsave("plots/usscaled_2020_03_26.png",
       p7, width = 6, height = 4)
ggsave("plots/ushex_2020_03_26.png",
       p8, width = 6, height = 4)
}

