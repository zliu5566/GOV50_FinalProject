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
library(MASS)
library(rstanarm)

d <- read_csv("treaties_data_Nov3.csv") %>% 
    mutate(number = map_dbl(document, ~ case_when(is.na(.) == TRUE ~ 0,
                                              TRUE ~ 1))) %>% 
    group_by(year) %>% 
    mutate(number = sum(number)) %>% 
    dplyr::select(-title, -date, -document, -dollars) %>% 
    mutate(pres_party = str_replace(pres_party, "D", "Democratic")) %>% 
    mutate(pres_party = str_replace(pres_party, "R", "Republican")) %>% 
    mutate(congress_party = str_replace(congress_party, "D", "Democratic")) %>% 
    mutate(congress_party = str_replace(congress_party, "R", "Republican")) %>%
    mutate(action_type = na_if(action_type, "Not Applicable")) %>%
    mutate(topic = str_replace(topic, "Extradition and Criminal Assistance",
                               "Extrad. and Criminal Assistance")) %>%
    mutate(topic = str_replace(topic, "International Law and Organization",
                               "Int. Law and Organization")) %>%
    mutate(topic = str_replace_all(topic,
                    c("Agriculture" = "Agriculture",
                      "Arms Control" = "Arms Ctrl",
                      "Aviation" = "Aviation", 
                      "Commercial" = "Commerce",
                      "Consular" = "Consular",
                      "Dispute Settlement and Arbitration" = "Arbitr",
                      "Drugs/Illegal Substances" = "Drugs",
                      "Environment" = "Enviro",
                      "Extradition" = "Extradition",
                      "Extrad. and Criminal Assistance" = "Extradition",
                      "Fisheries and Wildlife" = "Fish & Wild",
                      "Human Rights" = "Hum. Rights",
                      "Intellectual Property/Copyrights" = "Intel. Prop",
                      "International Law" = "Int. Law",
                      "Int. Law and Organization" = "Int. Law",
                      "Investment" = "Invest",
                      "Labor" = "Labor",
                      "Maritime Boundaries and Claims" = "Maritime",
                      "Mutual Legal Assistance" = "MLAT",
                      "Shipping and Marine Pollution" = "Maritime",
                      "Taxation" = "Tax",
                      "Telecommunications" = "Telecom",
                      "Terrorism" = "Terror",
                      "Trademarks/Patents" = "Intel. Prop",
                      "United Nations" = "UN"))) %>% 
    mutate(congress = factor(congress)) %>% 
    mutate(pres_congress = 
               case_when(pres_party == "Democratic" & 
                             congress_party == "Democratic" ~ "Both Dem.",
                         pres_party == "Republican" & 
                             congress_party == "Republican" ~ "Both Rep.",
                         pres_party == "Democratic" & 
                             congress_party == "Republican" ~ 
                             "Dem. President \n Rep. Senate",
                         TRUE ~ "Rep. President \n Dem. Senate")) %>% 
    rename("Year" = year, "President Party" = pres_party, 
           "President" = president, "Congress" = congress, 
           "Senate Party" = congress_party, "Treaty Topic" = topic,
           "Senate Action" = senate_action, 
           "Nat. Defense Spending ($ in Mil.)" = new_dollars,
           "Number of Treaties in the Year" = number,
           "President and Senate Political Parties" = pres_congress)

columns <- c("President", "Treaty Topic", "President Party", "Congress",
               "Senate Party", "President and Senate Political Parties")

fit_2 <- stan_glm(data = d %>% 
                      mutate(spending = 
                                 `Nat. Defense Spending ($ in Mil.)`/10000), 
                  `Number of Treaties in the Year` ~ 
                      spending + `President Party` + `Senate Party`,
                  refresh = 0,
                  family = poisson())

fit_2a <- fit_2 %>% 
    as_tibble() %>% 
    rename("intercept" = `(Intercept)`)

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
               party of the Senate for each Congress. Finally, I added to this 
               dataset national secutiry spending data from 1949 to the present,
               which I acquired from Whitehouse.gov. I then used the package 
               PriceR to adjust for inflation."),
             h3("About the Author"),
             p("My name is Z. Liu, and I study many things.
               You can reach me when this project is done.")),
    tabPanel("Graphics",
             titlePanel("Graphics"),
             h3("Explore Features of U.S. Treaties"),
             p("Select the values that you want to be plotted on the x and y 
               axes respectively and the type of graph you want produced. The 
               fill colors correspond to the actions taken by the Senate for 
               every treaty, while the alpha reflects the number of treaties 
               within each value on the x axis."),
             fluidPage(
                 selectInput("x", "X variable", columns),
                 selectInput("y", "Y variable", columns),
                 selectInput("geom", "geom", c("point", "column", 
                                               "jitter", "bar")),
                 plotOutput("plot2")
                 )),
    tabPanel("Model",
             titlePanel("Model"),
             h3("Military Spending vs. Treaties Signed: 1949-2020"),
             fluidPage(
                 plotOutput("plot1")
             ),
             p("What we see here is that there seems to be a negative 
               correlation between national defense spending and the number of 
               treaties ratified by the Senate, especially since the War on 
               Terror and during the Reagan administration."),
             h3("Building a Model"),
             fluidPage(
                 plotOutput("plot3")
                 )
             )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(aes(y = fct_reorder(.data[[input$y]],
                                                      .data[["Year"]]))),
               column = 
                   geom_col(aes(y = fct_reorder(.data[[input$y]], 
                                                .data[["Year"]]),
                                alpha = subset(d, !is.na(action_type)) %>% 
                                    group_by(input$y) %>% 
                                    mutate(number = n()) %>% 
                                    pull(number)),
                            position = "dodge"),
               jitter = geom_jitter(aes(y = fct_reorder(.data[[input$y]], 
                                                        .data[["Year"]])), 
                                    width = 0.2, 
                                    height = 0.2, alpha = 0.5),
               bar = geom_bar()
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
        ggplot(data = subset(d, !is.na(action_type)), 
               aes(fct_reorder(.data[[input$x]], .data[["Year"]]),
                   color = action_type, fill = action_type)) +
            plot_geom() +
            scale_fill_discrete(name = "Senate Action Taken") +
            scale_color_discrete(name = "Senate Action Taken") +
            scale_alpha_continuous(name = "Number of Treaties") +
            theme_bw() +
            theme(axis.text.x = element_text(size = 7.5, angle = -90),
                  axis.text.y = element_text(size = 7.5)) +
            labs(x = input$x,
                 y = ifelse(input$geom == "bar", "Count", input$y))
    }, res = 96)
    
    output$plot3 <- renderPlot({
        ggplot(data = fit_2a, aes(spending)) +
            geom_histogram(aes(y = after_stat(count/sum(count))),
                           bins = 100,
                           color = "white") +
            theme_bw() +
            labs(title = "Posterior Distribution for the Coefficient of 'spending'",
                 subtitle = "'spending': annual military spending in tens of billions $",
                 x = "Coefficient of 'spending'",
                 y = "Probability",
                 caption = "Used 'stan_glm' with the Poisson distribution") +
            scale_y_continuous(labels = scales::percent_format())
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
