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

party <- c("Democratic", "Republican")

fit_2 <- stan_glm(data = d %>% 
                      mutate(spending = 
                                 `Nat. Defense Spending ($ in Mil.)`/10000), 
                  `Number of Treaties in the Year` ~ 
                      spending + `President Party`*`Senate Party`,
                  refresh = 0,
                  family = poisson())

fit_2a <- fit_2 %>% 
    as_tibble()

fit_2b <- fit_2 %>% 
    as_tibble() %>%
    rename("Republican President" = "`President Party`Republican") %>%
    rename("Republican Senate" = "`Senate Party`Republican") %>%
    rename("Interaction" = 
               "`President Party`Republican:`Senate Party`Republican") %>% 
    rename("Intercept" = "(Intercept)") %>% 
    rename("Military Spending" = "spending")

fit_3 <- glm.nb(data = d,
                `Number of Treaties in the Year` ~ 
                    `Nat. Defense Spending ($ in Mil.)` + 
                    `President Party`*`Senate Party`)

# Define UI for application that draws a histogram
ui <- navbarPage(
    "[Project Title Signed but Pending Senate Ratification]",
    tabPanel("Introduction", 
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
               PriceR to adjust for inflation.")),
    tabPanel("Graphics",
             titlePanel("Visualizing Data"),
             h3("Military Spending vs. Treaties Signed: 1949-2020"),
             fluidPage(
                 plotOutput("plot1")
             ),
             h3("Correlation Between Spending and Treaties"),
             fluidPage(
                 plotOutput("plot4")
             ),
             h3("Explore Features of U.S. Treaties"),
             p("Select the values that you want to be plotted on the x and y 
               axes respectively and the type of graph you want produced. The 
               fill colors correspond to the actions taken by the Senate for 
               every treaty, while the alpha reflects the number of treaties 
               within each value on the x axis."),
             sidebarPanel(fluidPage(
                 selectInput("x", "X variable", columns),
                 selectInput("y", "Y variable", columns),
                 selectInput("geom", "geom", c("point", "column", 
                                               "jitter", "bar")))
                 ),
             fluidPage(
                 plotOutput("plot2")
                 )
             ),
    tabPanel("Model",
             titlePanel("Model"),
             p("The model formula would likely look like this:"),
             fluidPage(
                 withMathJax(),
                 helpText("$$\\text{Treaty Number}_i = \\beta_0 +
                 \\beta_1\\cdot\\text{Military Spending}_i + 
                 \\beta_2\\cdot\\text{Rep. Pres.}_i + 
                 \\beta_3\\cdot\\text{Rep. Sen.}_i +
                 \\beta_4\\cdot\\text{Rep. Pres.}_i\\cdot\\text{Rep. Sen.}_i +
                          \\epsilon_i$$")
             ),
             h3("Model Coefficient Posteriors"),
             p("Select the model variable to see the posterior distribution of 
               its coefficient"),
             fluidPage(
                 selectInput("v", "Variables", names(fit_2b))
                 ),
             fluidPage(
                 plotOutput("plot3")
                 ),
             h3("Using the Model"),
             p("Select the party of the President, the majority party of the 
               Senate, and the military spending in tens of billions of dollars
               to see my model's predictions for the number of treaties to be
               signed in such a year."),
             fluidPage(
                 selectInput("a", "President Party", party),
                 selectInput("b", "Senate Party", party),
                 sliderInput("c", "Military Spending in Tens of Billions $",
                             min = 0, max = 100, value = 50, round = FALSE),
                 plotOutput("plot5")
                 )
             ),
    tabPanel("About",
             titlePanel("About the Author"),
             p("My name is Z. Liu, and I study many things.
               You can reach me when this project is done."))
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
                     color = "white", fill = "deepskyblue3", 
                     position = "dodge") +
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
        ggplot(data = fit_2b, aes(.data[[input$v]])) +
            geom_histogram(aes(y = after_stat(count/sum(count))),
                           bins = 100,
                           color = "white",
                           fill = "darkorange2") +
            theme_bw() +
            labs(title = paste("Posterior Distribution of the Coefficient of",
                               input$v),
                 subtitle = paste(
                     case_when(input$v == "Military Spending" ~ 
                                   "Military Spending: in tens of billions $",
                               input$v == "Intercept" ~
                                   "Intercept: Dem. President and Dem. Senate",
                               input$v == "Republican President" ~ 
                                   "Republican President (no interaction)",
                               input$v == "Republican Senate" ~ 
                                   "Republican Senate (no interaction)",
                               TRUE ~ 
                                   "Interaction: GOP President and GOP Senate")
                     ),
                 x = paste("Coefficient of", input$v),
                 y = "Probability",
                 caption = "Used 'stan_glm' with the Poisson distribution.") +
            scale_y_continuous(labels = scales::percent_format())
    }, res = 96)
    
    output$plot4 <- renderPlot({
        ggplot(data = d, aes(d$`Nat. Defense Spending ($ in Mil.)`/10000, 
                             d$`Number of Treaties in the Year`)) +
            geom_point() +
            geom_smooth(color = "red", alpha = 0.2) +
            theme_bw() +
            labs(x = "Military Spending in Tens of Billions $",
                 y = "Number of Treaties Signed that Year")
    }, res = 96)
    
    output$plot5 <- renderPlot({
        new_obs <- tibble(`President Party` = input$a,
                          `Senate Party` = input$b,
                          spending = input$c)
        
        pp <- posterior_predict(fit_2, newdata = new_obs) %>% 
            as_tibble() %>% 
            mutate_all(as.numeric)
        
        pp %>% 
            ggplot(aes(`1`)) +
            geom_histogram(aes(y = after_stat(count/sum(count))),
                           binwidth = 1,
                           color = "white",
                           fill = "forestgreen") +
            theme_bw() +
            labs(title = "Posterior Probability Distributions",
                 subtitle = paste("For a", input$a, "presidency,", 
                                  "a", input$b, "Senate, and at $", 
                                  input$c*10, "billion"),
                 x = "Number of Treaties Signed",
                 y = "Probability") +
            coord_cartesian(xlim = c(0, 100), ylim = c(0, 0.2)) +
            scale_y_continuous(labels = scales::percent_format()) +
            scale_x_continuous(breaks = seq(0, 100, by = 10))
    }, res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
