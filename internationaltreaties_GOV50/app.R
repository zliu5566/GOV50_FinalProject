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

d <- read_csv("treaties_data_Oct15.csv") %>% 
    select(-title, -document, -date)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "[Project Title Signed but Pending Senate Ratification]",
    tabPanel("About", 
             titlePanel("About My Project"),
             h3("Project Overview"),
             p("Hello, this project looks at U.S. international treaties from
               1949 to 2020. Specifically, it examines the realtionship between
               the U.S. Senate's willingness to pass a treaty and such factors 
               as the sitting U.S. President, the dominant party in the Senate,
               and whether or not one party holds both branches. In addition, 
               this project also seeks to discover if there is any correlation 
               between the Senate's willingness to ratify a treaty and the 
               dollar amount of money it allocates for national defense 
               purposes. I have always been fascinated by U.S. foreign policy, 
               and international treaties seem to be one of the most 
               prominent ways in which foreign policy is enacted."),
             h3("Data Collection"),
             p("I collected this data by scraping Congress.gov, which has the 
               voting records of the U.S. Senate for every treaty that was 
               submitted to that body since 1949. I then added to that dataset
               information about presidential terms and the dominant political
               party of the Senate for each Congress."),
             h3("About the Author"),
             p("My name is Z. Liu, and I study many things. 
             You can reach me when this project is done.")),
    tabPanel("Model",
             titlePanel("Model and Graphics"),
             fluidPage(
                 selectInput("x", "X variable", names(d)),
                 selectInput("y", "Y variable", names(d)),
                 selectInput("geom", "geom", c("point", "column", "jitter")),
                 plotOutput("plot")
             ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(),
               column = geom_col(),
               jitter = geom_jitter(width = 0.2, height = 0.2, alpha = 0.5)
        )
    })

    output$plot <- renderPlot({
        ggplot(d, aes(.data[[input$x]], .data[[input$y]])) +
            plot_geom()
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
