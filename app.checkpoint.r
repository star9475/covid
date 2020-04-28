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
library(ggrepel)

options(scipen=999)

source('covid.r')

states <- data_all %>%
  arrange(match(state, c("WA", "ID", "NY", "LA")), state) %>%
  select (state)

# Define UI 
ui <- fluidPage(
   
   # Application title
  #titlePanel(h1("COVID-19 Plots"),"Data courtesy of https://covidtracking.com"),
  h2("COVID-19 Plots"),
  
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
    
      selectInput("select_state", label = "Select State", 
                   choices = states$state,
                   selected = 1),

      checkboxGroupInput("select_cum", "State Cumulative Data", 
                         choiceNames = c(cum_data),
                         choiceValues = c(cum_data),
                         selected = cum_data[[1]][1], inline = TRUE),
      checkboxGroupInput("select_daily", "State Daily Data", 
                         choiceNames = c(daily_data),
                         choiceValues = c(daily_data),
                         selected = daily_data[[1]][1],inline = TRUE),
      radioButtons("select_unit2", "Report By Days or Weeks:",
                   c("Days" = "days",
                     "Weeks" = "weeks")),
      "Note: Last week may not be full 7 days.",
      hr(),
      radioButtons("select_unit", "US Scaled to Population:",
                   c("Normal" = "norm",
                     "Scaled" = "scale")),


       # radioButtons("plot_type", "US Plot type:",
       #              c("Normal" = "norm",
       #                "Facet" = "facet") ),
      hr(),
      "Josh Starkey : @starkeeey",
      hr(),
      "Data courtesy of https://covidtracking.com",
      br(), "If a measurement isn't being shown on the graph it's due to that state not reporting it."
       
     ),
      
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("States", plotOutput("cumPlot"), # ),
                    hr(),
                    plotOutput("dailyPlot") ),
                    tabPanel("US", plotOutput("usPlot") )

        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$cumPlot <- renderPlot({
     st <- input$select_state
     features <- input$select_cum
     unit <- input$select_unit2

     if (st == "") {st == "WA"}
     p1 <- get_state_plot(data_all, st, features, type = "line", unit)
     p1
   })
   
   hr()

   output$dailyPlot <- renderPlot({
     st <- input$select_state
     features <- input$select_daily
     unit <- input$select_unit2
     
     if (st == "") {st == "WA"}
     p2 <- get_state_plot(data_all, st, features, type = "bar", unit)
     p2
   })
   
   output$usPlot <- renderPlot({

     unit <- input$select_unit
     p3 <- get_us_line_plot(data_all, unit)    
     p3
   })
#   output$usFacet <- renderPlot({
#     p4 <- get_us_facet(data_all)
#     p4
#   })
}

# Run the application 
shinyApp(ui = ui, server = server)

