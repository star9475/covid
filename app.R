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


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(h3("COVID-19 Plots"),"Data from"),
  br(),
  #"Josh Starkey : @starkeeey",
  "Data courtesy of https://covidtracking.com", 
  br(),
  br(),
  tabsetPanel(
    
    tabPanel("States", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("select_state", label = "Select State", 
                             choices = states$state,
                             selected = 1),
                 
                 checkboxGroupInput("select_cum", "Cumulative Data (top chart)", 
                                    choiceNames = c(cum_data),
                                    choiceValues = c(cum_data),
                                    selected = cum_data[[1]][1], inline = TRUE),
                 checkboxGroupInput("select_daily", "Daily Increases (bottom chart)", 
                                    choiceNames = c(daily_data),
                                    choiceValues = c(daily_data),
                                    selected = daily_data[[1]][1],inline = TRUE),
                 radioButtons("select_unit2", "Report By Days or Weeks:",
                              c("Days" = "days",
                                "Weeks" = "weeks")),
                 "Note: Last week may not be full 7 days.",
                 br(), br(), "If a measurement isn't being shown on the graph it's due to that state not reporting it."
                 
               ),
               mainPanel(
                 plotOutput("cumPlot"),
                 hr(),
                 plotOutput("dailyPlot")
               )
             )
    ),
    tabPanel("US", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("select_state2", label = "Select State", 
                             choices = states$state,
                             selected = "WA",
                             multiple = TRUE),
                 radioButtons("select_chart", "Chart Type:",
                              c("Line" = "line",
                                "Facet (All States)" = "facet")),
                 radioButtons("select_unit", "US Scaled to Population:",
                              c("Normal" = "norm",
                                "Scaled" = "scale"))
               ),
               mainPanel(fluidRow(
                 plotOutput("usPlot")   
               )
               )
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
    chart <- input$select_chart
    st <- input$select_state2
    
    if (st == "") {st == "WA"}
    
    if (chart == "line") {
      p3 <- get_us_line_plot(data_all, type = unit, st) 
    }
    else p3 <- get_us_facet_plot(data_all)
    p3
  })
  #   output$usFacet <- renderPlot({
  #     p4 <- get_us_facet(data_all)
  #     p4
  #   })
}
# Run the application 
shinyApp(ui = ui, server = server)
