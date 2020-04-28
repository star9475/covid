#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(usmap)
library(ggplot2)

options(scipen=999)

### Preprocessing
data_pop <- usmap::statepop %>% 
  #  rownames_to_column() %>% 
  rename(Population = pop_2015) %>%
  rename(state = abbr)


data <- read_csv(url("https://covidtracking.com/api/states/daily.csv"))

#saveRDS(data, "data_covidtracking.rds")
#data <- readRDS("data_covidtracking.rds")
data_all <- data %>%
  mutate(date = ymd(date)) %>%
  mutate(label = if_else(date == max(date, na.rm=TRUE), state, NA_character_)) %>%
  inner_join(data_pop) %>%
  mutate(pos_test_per = positive / Population) %>%
  mutate(state = factor(state)) %>%
  mutate(date = if_else( (state == "WA"| state =="ID"), date - 1, date))




# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("COVID-19 Plots"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       # selectInput("select_state", label = "Select QB for Pass Locations", 
       #             qbs$name, 
       #             selected = 1),         
       selectInput("select_state", label = "Select State", 
                   choices = data_all$state, 
                   selected = NULL)
     ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("barPlot"),
         hr(),
         plotOutput("usPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$barPlot <- renderPlot({
     input$select_state
     #st <- input$select_state
     st <- ifelse(input$select_state != "",input$select_state, "WA")
     #st <- "WA"
     
    data_st <- data_all %>%
       filter(state == st) %>%
       arrange(date) %>%
       mutate( ppd= positive-lag(positive)) %>%
       mutate( npd = negative-lag(negative))
     data_st[1,]$ppd = data_st[1,]$positive
     
     ### State Plot
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
     
   })
   hr()

   output$usPlot <- renderPlot({
     st <- input$select_state
     
     data_all %>%
       #filter(state == st) %>%
     ggplot( aes(x=date, y=pos_test_per, group=1)) +  
       #geom_path() +
       geom_path(size=.3, color = "#2c7bb6") + #geom_smooth(size=.3)+
        theme_minimal() +
       theme(axis.text.x = element_text(vjust = 0.5, angle = 90)) +
       theme(axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank())+       
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             panel.grid.minor = element_blank())+
       facet_wrap(~ state) +
       labs(#x = "", y = "Importance", 
         title = "Confirmed COVID-19 cases per state scaled for population",
         subtitle = paste0("Data Updated: ",max(data_all$dateChecked)),
         caption = "Data: https://covidtracking.com/ | Plot: @starkeeey") 
       
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

