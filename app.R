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

source('covid.R')

states <- data_all %>%
  arrange(match(state, c("WA", "ID")), state) %>%
  select (state)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  #titlePanel(h1("COVID-19 Plots"),"Data courtesy of https://covidtracking.com"),
  h1("COVID-19 Plots"),
  
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
    
       selectInput("select_state", label = "Select State", 
#                   choices = data_all$state,
                   choices = states$state,
                   selected = 1),
       # selectInput("select_state", label = "Select State", 
       #             choices = data_all$state, 
       #             selected = NULL),
       hr(),
       radioButtons("plot_type", "US Plot type:",
                    c("Normal" = "norm",
                      "Facet" = "facet") ),
       hr(),
       "Josh Starkey : @starkeeey",
       hr(),
       "Data courtesy of https://covidtracking.com"
       
     ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    tabPanel("States", plotOutput("barPlot") ),

                    tabPanel("US", plotOutput("usPlot") )
                    
#                    tabPanel("US Facet", plotOutput("usFacet"))
        )
        
         

      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$barPlot <- renderPlot({
     st <- input$select_state
     #cat(st)
     if (st == "") {cat("YES")}
     if (st == "") {st == "WA"}
     p3 <- get_state_plot_ppd_npd(data_all, st)
     p3
   })
   
   hr()

   output$usPlot <- renderPlot({
     pt <- input$plot_type
      switch(pt,
             "facet" = get_us_facet(data_all),
             "norm" = get_us_linechart_scaled(data_all)
             )
      
     
     #p7 <- get_us_linechart_scaled(data_all)    
     #p7
   })
#   output$usFacet <- renderPlot({
#     p4 <- get_us_facet(data_all)
#     p4
#   })
}

# Run the application 
shinyApp(ui = ui, server = server)

