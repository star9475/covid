library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyr)
library(ggrepel)
library(geofacet)
library(purrr)
#library(curl)

RUNCHARTS = FALSE
DEBUG = FALSE

#import US state populations from US Census Bureau
## future addon.  need to convert states to abbrevs
# temp <- tempfile()
# source <- "http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv"
# temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
# US1019pop <- read.csv(temp)[,c(5,8:17)]
# colnames(US1019pop) <- c("State", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")

##### Get the state Pop Data
data_population <- usmap::statepop %>% 
  #  rownames_to_column() %>% 
  rename(Population = pop_2015) %>%
  rename(state = abbr)

## Whilst debugging, let's not crush the covidtracker website
if (DEBUG == FALSE) {
  data <- read_csv(url("https://covidtracking.com/api/states/daily.csv"))
  saveRDS(data, "data_covidtracking.rds")
} else {
  data <- readRDS("data_covidtracking.rds")
}

data_all <- data %>%
  rename("inIcu" = "inIcuCumulative", 
         "onVentilator" = "onVentilatorCumulative") %>%
  mutate(date = ymd(date),
         dateChecked = as_date(dateChecked)) %>%
  inner_join(data_population) %>%
  mutate(pos_test_per = positive / Population,
         percent_pos = positive / totalTestResults) %>%
  mutate(state = factor(state)) %>%
  mutate(date = if_else( (state == "WA"| state =="ID"), date - 1, date)) %>%
  group_by(state) %>%
  arrange(date) %>%
  mutate( positive_inc = positive-lag(positive),
          negative_inc = negative-lag(negative),
          hospitalized_inc = hospitalized-lag(hospitalized),
          inIcu_inc = inIcu-lag(inIcu),
          onVentilator_inc = onVentilator - lag(onVentilator),
          death_inc = death-lag(death),
          pending_inc = pending-lag(pending),
          total_inc = total-lag(total)) %>%
  ungroup() 

#### Features we want to plot, grouped by two types cumulative and daily
cum_data <- c("positive",
              "negative", 
              "hospitalized", 
              "inIcu", 
              "onVentilator",
              "death", 
              "pending", 
              "recovered" ,
              "total", 
              "percent_pos")

daily_data <- colnames(select(data_all,ends_with("inc")))

get_state_plot <- function(d, st = "WA", selections, type = "line", unit = "weeks") {

    data_to_plot <- data_all %>%
      filter(state == st) %>%
      select(date,  one_of(selections)) %>%
      gather(test,results,2:length(.)) %>% 
      group_by(test) %>% 
      mutate(end_value = last(na.omit(results)) )%>%
      mutate(end_label = ifelse(date == max(date), round(end_value,3), "" )) %>%
#      mutate(end_label = ifelse(date == max(date), end_value, NA_character_ )) %>%
      ungroup() 
 
    if (unit == "weeks") {     
      data_to_plot <- data_to_plot %>%
        group_by(date = week(date), test) %>%
        group_split(date,test) %>% #Previously we had to do split(.$test) here
        map_dfr(. %>%
                  summarise(test = first(test), 
                            date = first(date),
                            results = if_else(stringr::str_detect(test,"per"),mean(results),sum(results)))) %>%
        group_by(test) %>%
        mutate(end_value = last(na.omit(results)),
               end_label = ifelse(date == max(date), round(end_value,3), "" )) %>%
        ungroup() %>%
        mutate(date = date - min(date)) ##force week to start at 0
      
    }
    
    ## Intial Plot based on type
    if (type == "line") {
      p <- ggplot(data = data_to_plot,
                mapping = aes(x = date, y = results, color = test, label = end_label)) +
      geom_line(size = 1) +
      geom_point(size=3, shape=16, alpha = .6) +
#      geom_text(aes(y = data_to_plot$end_label, label = data_to_plot$end_label))+
      geom_text_repel(
          force=1,
          point.padding=unit(1,'lines'),
          direction='y',
          nudge_x=0.1,
          segment.size=0.2,
          segment.color = "grey50"
          #label = round(data_to_plot$end_label,2)
        ) +
        labs(title = paste0("COVID-19 Daily Data : ",st))
    } else { #bar chart
      p <- ggplot(data = data_to_plot,
              mapping = aes(x = date, y = results,
                            fill = test, label = end_label)) +
        geom_bar(stat="identity",  position = "dodge") +
        geom_text(aes(label=end_label), position=position_dodge(width=0.9), vjust=-0.25) +
#        geom_text(aes(label = end_label, vjust=-0.5)) +
        scale_y_continuous(limits=c(0,max(data_to_plot$results*1.2))) +
        labs(title = paste0("COVID-19 Daily Data : ",st))
      }

    if (unit == "weeks") {
      p <- p + scale_x_continuous(breaks = seq(0, max(data_to_plot$date), by = 1))
    }
    else { ## Scale is days
      p <- p + scale_x_date(date_breaks = "1 week" , date_labels = "%b-%d") +
        theme(axis.text.x = element_text(vjust = 0.5, angle = 90))
    }


    p <- p  +
      scale_color_brewer(palette="Set1") +
      scale_fill_brewer(palette="Set1") +
#      scale_fill_brewer(palette="Dark2") +
#      scale_color_brewer(palette="Dark2") +

      theme_minimal() +
      labs(x = "Date", y = "Reported Events",
           subtitle = paste0("Data Updated: ",format(max(data_all$date),'%b %d, %Y'))) +
      theme(legend.title = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "top")

}

get_us_line_plot <- function(d, type = "norm", selections = "WA") {
  
  ### Selection is for normal data or scaled to population
  if(type == "norm") {
    d <- mutate(d, unit = positive)
  }else {
    d<-  mutate(d, unit = pos_test_per)
  }
  
  #cat(selections)
#  data_to_highlight <- d %>% group_by(state) %>% summarise(tot = max(unit)) %>% arrange(desc(tot)) %>% head(8)
  data_to_highlight <- d %>% 
    group_by(state) %>% 
    summarise(tot = max(unit)) %>% 
    arrange(desc(tot)) %>% 
    filter(state %in% selections)
  
  ## Get the Top 8 to highlight in the chart
  data_highlighted <- d %>%
    group_by(state) %>%
    filter(positive > 0) %>%
    arrange(date) %>%
    mutate(ppd_na = na_if(positive_inc,0)) %>%
    mutate(days_elapsed = date-min(date),
           end_label = if_else(date == max(date), as.character(state), NA_character_) ) %>% 
    select(date, state, positive, positive_inc, days_elapsed, end_label, unit) %>%
    filter(state %in% data_to_highlight$state) %>%
    droplevels() 
  
  ## Plot remaining states as gray
  data_gray <- d %>%
    group_by(state) %>%
    filter(positive > 0) %>%
    arrange(date) %>%
    mutate(ppd_na = na_if(positive_inc,0)) %>%
    mutate(days_elapsed = date-min(date)) %>% 
    select(date, state, positive, positive_inc, days_elapsed, unit) %>%
    filter(! state %in% data_to_highlight$state) %>%
    droplevels()
  
  ggplot() +
    geom_line(data = data_highlighted, 
              aes(x=days_elapsed, y=unit, 
                  group = state, 
                  color = state, 
                  label=end_label),
              size =1) +
    geom_point(data = data_highlighted, 
               aes(x=days_elapsed, y=unit, 
                   group = state,
                   color = state),
               size=3, shape=16, alpha = .6) +
#    size = 2) +
  geom_line(data = data_gray, 
              aes(x=days_elapsed, y=unit, group=state), 
              colour = alpha("grey", 0.7) ,
              size = 0.5 ) +
    scale_x_continuous(limits = c(10, max(data_highlighted$days_elapsed))) +
    geom_label_repel(data = data_highlighted,
                     aes(x=days_elapsed, y=unit, color=state),
                     label = data_highlighted$end_label,
                     force = 1,
                     point.padding = 0.1,
                     segment.size = 0.2, size=3) +
    
#    scale_color_brewer(palette="Set1") +
    scale_color_brewer(palette="Dark2") +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(panel.grid.minor = element_blank()) +
    labs(x = "Days since first reported positive test", 
         y = "Positive Tests", 
         title = "Total Confirmed COVID-19 : United States",
         subtitle = paste0("Data Updated: ",format(max(data_all$dateChecked),'%b %d, %Y')),
         caption = "Data: https://covidtracking.com/ | Plot: @starkeeey")
  
}

get_us_facet_plot <- function(data) {
  data %>%
    ggplot( aes(x=date, y=positive_inc)) +  
    #geom_path() +
    geom_bar(stat="identity",fill = "#2c7bb6")+ 
    theme_minimal() +
    theme(axis.text.x = element_text(vjust = 0.5, angle = 90)) + theme(legend.position = "none")+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+       
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.minor = element_blank())+
    facet_geo(~ state, grid = "us_state_grid2") +
    theme(strip.text.x = element_text(size = 8))+
    labs(#x = "", y = "Importance", 
      title = "Confirmed COVID-19 cases per day",
      subtitle = paste0("Data Updated: ",max(data_all$dateChecked)),
      caption = "Data: https://covidtracking.com/ | Plot: @starkeeey")   
  }

if (RUNCHARTS) {
  
  p1 <- get_state_plot(data_all, st = "WA", selections = c("positive_inc", "total_inc"), type = "bar")
  p2 <- get_state_plot(data_all, st = "ID", selections = c("positive_inc", "total_inc"), type = "bar")
  p3 <- get_us_line_plot(data_all, "scale")
  p4 <- get_us_facet_plot(data_all)
  ggsave(paste0("plots/wa_",lubridate::today(),".png"),
         p1, width = 6, height = 4)  
  ggsave(paste0("plots/id_",lubridate::today(),".png"),
         p2, width = 6, height = 4)
  ggsave(paste0("plots/us_",lubridate::today(),".png"),
         p3, width = 6, height = 4)
  ggsave(paste0("plots/facet_",lubridate::today(),".png"),
         p4, width = 7, height = 5)
}