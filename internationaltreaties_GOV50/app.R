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
library(priceR)

d <- read_csv("treaties_data_Oct29.csv") %>% 
    mutate(number = map_dbl(document, ~ case_when(is.na(.) == TRUE ~ 0,
                                              TRUE ~ 1))) %>% 
    group_by(year) %>% 
    mutate(number = sum(number)) %>% 
    select(-title, -date, -document, -dollars) %>% 
    mutate(pres_party = str_replace(pres_party, "D", "Democratic")) %>% 
    mutate(pres_party = str_replace(pres_party, "R", "Republican")) %>% 
    mutate(congress_party = str_replace(congress_party, "D", "Democratic")) %>% 
    mutate(congress_party = str_replace(congress_party, "R", "Republican")) %>% 
    rename("Year" = year, "President Party" = pres_party, 
           "President" = president, "Congress" = congress, 
           "Senate Party" = congress_party, "Treaty Topic" = topic,
           "Senate Action" = senate_action, 
           "Nat. Defense Spending ($ in Mil.)" = new_dollars,
           "Number of Treaties in the Year" = number)

columns_x <- c("President", "President Party", "Congress", "Senate Party")

columns_y <- c("President", "Treaty Topic", "President Party", "Congress",
               "Senate Party")

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
             titlePanel("Models and Graphics"),
             h3("Military Spending vs. Treaties Signed: 1949-2020"),
             fluidPage(
                 plotOutput("plot1")
                 ),
             h3("Make Your Own Graphic"),
             fluidPage(
                 selectInput("x", "X variable", columns_x),
                 selectInput("y", "Y variable", columns_y),
                 selectInput("geom", "geom", c("point", "column", 
                                               "jitter", "line")),
                 plotOutput("plot2")
                 )
             )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(aes(y = .data[[input$y]])),
               column = 
                   geom_col(
                       aes(y = .data[[input$y]],
                           alpha = d %>%
                               mutate(number = map_dbl(action_type, 
                                       ~ case_when(. == "Not Applicable" ~ 0,
                                                   TRUE ~ 1))) %>%
                               group_by(input$y) %>%
                               mutate(number_2 = sum(number)) %>% 
                               pull(number_2)),
                                 position = "dodge"),
               jitter = geom_jitter(aes(y = .data[[input$y]]), width = 0.2, 
                                    height = 0.2, alpha = 0.5),
               line = geom_line(aes(y = .data[[input$y]]))
        )
    })

    output$plot1 <- renderPlot({
        ggplot(d, aes(x = d$Year)) +
            geom_col(aes(y = d$"Number of Treaties in the Year"),
                     color = "white", fill = "dodgerblue", position = "dodge") +
            geom_line(aes(y = d$"Nat. Defense Spending ($ in Mil.)"/20000),
                      color = "red", size = 1.5) +
            scale_y_continuous(
                name = "Number of Treaties per Year",
                sec.axis = 
                    sec_axis(trans = ~ . * 20, 
                             name = "Billions of $ (adjusted to 2009 $)")
                ) +
            theme_bw() +
            labs(x = "Year")
    }, res = 96)
    
    output$plot2 <- renderPlot({
        ggplot(d, aes(.data[[input$x]], 
                      color = action_type, fill = action_type)) +
            plot_geom() +
            scale_fill_discrete(name = "Senate Action") +
            scale_color_discrete(name = "Senate Action") +
            scale_alpha_continuous(name = "Number of Treaties") +
            theme(axis.text.x = element_text(size = 7.5),
                  axis.text.y = element_text(size = 7.5))
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
