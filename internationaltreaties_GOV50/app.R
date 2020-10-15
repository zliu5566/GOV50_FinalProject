#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(ggforce)
library(readr)

d <- read_csv("https://projects.fivethirtyeight.com/2020-general-data/presidential_ev_probabilities_2020.csv",
              col_types = cols(cycle = col_double(),
                               branch = col_character(),
                               model = col_character(),
                               modeldate = col_character(),
                               candidate_inc = col_character(),
                               candidate_chal = col_character(),
                               candidate_3rd = col_logical(),
                               evprob_inc = col_double(),
                               evprob_chal = col_double(),
                               evprob_3rd = col_logical(),
                               total_ev = col_double(),
                               timestamp = col_character(),
                               simulations = col_double())) %>% 
    select(evprob_chal, evprob_inc, total_ev)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project Title",
    tabPanel("Home", 
             titlePanel("Welcome to my page"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h4("This heading is smaller"),
             p("Here is a link to",
               a("Google", href = "https://www.google.com"))),
    tabPanel("Model",
             fluidPage(
                 selectInput("x", "X variable", choices = names(d)),
                 selectInput("y", "Y variable", choices = names(d)),
                 selectInput("geom", "geom", c("point", "column", "jitter")),
                 plotOutput("plot")
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is ______ and I study ______. 
             You can reach me at ______@college.harvard.edu."))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               column = geom_col(),
               jitter = geom_jitter()
        )
    })
    
    output$plot <- renderPlot({
        ggplot(d, aes(.data[[input$x]], .data[[input$y]])) +
            plot_geom()
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
