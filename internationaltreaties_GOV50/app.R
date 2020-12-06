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
library(shinythemes)
library(gtsummary)
library(gt)
library(broom.mixed)

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

president_term <- tibble(start = c(1949, 1953, 1961, 1969, 1977, 1981, 1993,
                                   2001, 2009, 2017),
                         end = c(1953, 1961, 1969, 1977, 1981, 1993, 2001,
                                 2009, 2017, 2020),
                         base = rep(0, 10),
                         height = rep(40, 10),
                         party = c("D", "R", "D", "R", "D", "R", "D", "R", "D", 
                                   "R"))

d2 <- d %>% 
    mutate("Military Spending" = `Nat. Defense Spending ($ in Mil.)`/10000) %>%
    group_by(Year, `President Party`, `Senate Party`, `Military Spending`, 
             `Number of Treaties in the Year`, President) %>% 
    summarize("Number of Treaties" = mean(`Number of Treaties in the Year`),
              .groups = "drop") %>% 
    select(-`Number of Treaties in the Year`)

fit_1 <- stan_glm(data = d2, 
                  `Number of Treaties` ~ 
                      `Military Spending` + `President Party`*`Senate Party`,
                  refresh = 0,
                  family = neg_binomial_2())

fit_1a <- fit_1 %>% 
    as_tibble()

fit_1b <- fit_1 %>% 
    as_tibble() %>%
    rename("Republican President" = "`President Party`Republican") %>%
    rename("Republican Senate" = "`Senate Party`Republican") %>%
    rename("Interaction" = 
               "`President Party`Republican:`Senate Party`Republican") %>% 
    rename("Intercept" = "(Intercept)") %>% 
    rename("Military Spending" = "`Military Spending`")

fit_2 <- stan_glm(data = d2, 
                  `Number of Treaties` ~ 
                      `Military Spending` + `President Party`*`Senate Party`,
                  refresh = 0,
                  family = gaussian())

fit_3 <- stan_glm(data = d2, 
                  `Number of Treaties` ~ 
                      `Military Spending` + `President Party`*`Senate Party`,
                  refresh = 0,
                  family = poisson())

fit_4 <- stan_glm(data = d2, 
                  `Number of Treaties` ~ 
                      `Military Spending` + `President Party`*`Senate Party` +
                      President,
                  refresh = 0,
                  family = neg_binomial_2())

fit_5 <- glm.nb(data = d2,
                `Number of Treaties` ~ 
                    `Military Spending` + `President Party`*`Senate Party`)

fit_6 <- glm.nb(data = d2,
                `Number of Treaties` ~ 
                    `Military Spending` + `President Party`*`Senate Party` +
                    President)

# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme("lumen"),
    title = paste("Elephants, Donkeys, Doves, and Hawks:
                  Predicting U.S. Treaties"),
    tabPanel("Predicting Treaties", 
             titlePanel("Predicting U.S. International Treaties"),
             sidebarPanel(
                 fluidPage(
                   p("Set the following values to predict the number of treaties
                     that will be signed in such a year."),
                   selectInput("a", "President Party", party),
                   selectInput("b", "Senate Party", party),
                   sliderInput("c", "Military Spending in Tens of Billions $",
                               min = 0, max = 100, value = 50, 
                               round = FALSE)
                   ),
                 width = 4),
             mainPanel(
                 fluidPage(
                     plotOutput("plot5")
                     ),
                 width = 8),
             h3("Project Overview"),
             p("This project looks at U.S. international treaties from
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
             ),
    tabPanel("Graphics",
             tabsetPanel(
                 tabPanel("Visualizing the Data",
                          titlePanel("Visualizing Data Correlations"),
                          h3("Military Spending vs. Treaties Signed: 
                             1949-2020"),
                          fluidPage(
                              plotOutput("plot1")
                              ),
                          h3("Correlation Between Spending and Treaties"),
                          fluidPage(
                              plotOutput("plot4")
                              )),
                 tabPanel("Exploring the Data", 
                          titlePanel("Pick Your Own Data Visualization"),
                          sidebarPanel(fluidPage(
                              selectInput("x", "X variable", columns),
                              selectInput("y", "Y variable", columns),
                              selectInput("geom", "geom", c("point", "column", 
                                                            "jitter", "bar"))),
                              width = 3
                              ),
                          mainPanel(fluidPage(
                              plotOutput("plot2")
                              ),
                              width = 9)
                 ))
             ),
    tabPanel("Model",
             titlePanel("Model"),
             fluidPage(
                 withMathJax(),
                 helpText("$$\\text{Treaty Number}_i = \\beta_0 +
                 \\beta_1\\cdot\\text{Military Spending}_i + 
                 \\beta_2\\cdot\\text{Rep. Pres.}_i + 
                 \\beta_3\\cdot\\text{Rep. Sen.}_i +
                 \\beta_4\\cdot\\text{Rep. Pres.}_i\\cdot\\text{Rep. Sen.}_i +
                          \\epsilon_i$$")
                 ),
             p(" "),
             tabsetPanel(
                 tabPanel("Building the Model",
                          h3("Creating a Linear Model"),
                          p("The formula upon which I settled for this model 
                            includes: an intercept, military spending (in tens 
                            of billions of dollars, since the military spends 
                            too much to calculate the impact of individual 
                            dollars being spent), the party of the president, 
                            the party of the Senate majority, and an interaction
                            variable to account for the effects of having a 
                            president and a Senate majority being of the same 
                            party."),
                          h3("Model Coefficient Posteriors"),
                          sidebarPanel(fluidPage(
                              p("Select a variable within the model to see the 
                                posterior distribution of its coefficient"),
                              selectInput("v", "Variables", names(fit_1b))
                              ),
                          width = 4),
                          mainPanel(fluidPage(
                              plotOutput("plot3")
                              ),
                              width = 8)),
                 tabPanel("Explaining the Model",
                          h3("Interpreting the Numbers"),
                          p("The regression table of the model I built can be 
                            seen below:"),
                          fluidPage(
                              gt_output("table1")
                              ),
                          p("What we see here is that [Interpretation]")
                          ),
                 tabPanel("Selecting a Distribution",
                          h3("Negative Binomial"),
                          splitLayout(
                              cellWidths = c("55%", "45%"),
                              h4("Posterior Predictive Check: Mean"),
                              h4("Posterior Predictive Check: Distribution")
                              ),
                          fluidPage(
                              splitLayout(
                                  cellWidths = c("55%", "45%"),
                                  plotOutput("plot6"),
                                  plotOutput("plot7")
                                  )
                              ),
                          p("[Benefits of and problems with]"),
                          h3("Gaussian"),
                          splitLayout(
                              cellWidths = c("55%", "45%"),
                              h4("Posterior Predictive Check: Mean"),
                              h4("Posterior Predictive Check: Distribution")
                              ),
                          fluidPage(
                              splitLayout(
                                  cellWidths = c("55%", "45%"),
                                  plotOutput("plot8"),
                                  plotOutput("plot9")
                                  )
                              ),
                          p("[Benefits of and problems with]"),
                          h3("Poisson"),
                          splitLayout(
                              cellWidths = c("55%", "45%"),
                              h4("Posterior Predictive Check: Mean"),
                              h4("Posterior Predictive Check: Distribution")
                              ),
                          fluidPage(
                              splitLayout(
                                  cellWidths = c("55%", "45%"),
                                  plotOutput("plot10"),
                                  plotOutput("plot11")
                                  )
                              ),
                          p("[Benefits of and problems with]")
                          ),
                 tabPanel("An Alternative Model",
                          h3("Dummy Variable"),
                          p("No model, of course, is perfect. Hence, in addition 
                            to producing a model that takes into account 
                            military spending and political party, I created a 
                            second model that contains a dummy variable to 
                            account for the uniqueness of each president within 
                            the timespan of my data. The regression table of the 
                            new model can be seen below:"),
                          fluidPage(
                              gt_output("table2")
                              ),
                          p("[Explanation of what this model means]"),
                          splitLayout(
                              cellWidths = c("55%", "45%"),
                              h4("Posterior Predictive Check: Mean"),
                              h4("Posterior Predictive Check: Distribution")
                              ),
                          fluidPage(
                              splitLayout(
                                  cellWidths = c("55%", "45%"),
                                  plotOutput("plot12"),
                                  plotOutput("plot13")
                                  )
                              ),
                          p("[Explanation of what this means for fit]")
                          ),
                 tabPanel("A Non-Bayesian Approach",
                          h3("Using glm.nb"),
                          p("For the sake of thoroughness, I also decided to 
                            create a non-Bayesian model for my data using the 
                            glm.nb function, the regression table of which is 
                            below:"),
                          fluidPage(
                              gt_output("table3")
                              ),
                          p("[Explanation of what this model means]"),
                          h3("Dummy Variable (Part II)"),
                          p("Here is the glm.nb model with the president dummy
                            variable:"),
                          fluidPage(
                              gt_output("table4")
                              ),
                          p("[Explanation of what this model means]")
                          )
                 ),
             ),
    tabPanel("About",
             titlePanel("About this Project"),
             h3("Data Collection"),
             p("I collected this data by scraping Congress.gov, which has the 
               voting records of the U.S. Senate for every treaty that was 
               submitted to that body since 1949. I then added to that dataset
               information about presidential terms and the dominant political
               party of the Senate for each Congress. Finally, I added to this 
               dataset national security spending data from 1949 to the present,
               which I acquired from Whitehouse.gov. I then used the package 
               PriceR to adjust for inflation."),
             h3("About the Author"),
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
        ggplot() +
            geom_rect(data = president_term,
                      aes(xmin = start,
                          xmax = end,
                          ymin = base,
                          ymax = height,
                          fill = party),
                      alpha = 0.1) +
            geom_col(data = d,
                     aes(x = d$Year,
                         y = d$"Number of Treaties in the Year"),
                     color = "white", fill = "goldenrod3", 
                     position = "dodge") +
            geom_line(data = d,
                      aes(x = d$Year,
                          y = d$"Nat. Defense Spending ($ in Mil.)"/20000),
                      color = "red", size = 1.5) +
            scale_y_continuous(
                name = "Number of Treaties per Year",
                sec.axis = 
                    sec_axis(trans = ~ . * 20, 
                             name = "Billions of $ (adjusted to 2009 $)")
                ) +
            scale_fill_manual(name = "President Party",
                              labels = c("Democratic", "Red"),
                              values = c("blue", "red")) +
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
        ggplot(data = fit_1b, aes(.data[[input$v]])) +
            geom_histogram(aes(y = after_stat(count/sum(count))),
                           bins = 100,
                           color = "white",
                           fill = "darkgoldenrod2") +
            geom_vline(xintercept = median(fit_1b[[input$v]]), color = "red") +
            geom_vline(xintercept = 
                           median(fit_1b[[input$v]]) + sd(fit_1b[[input$v]])*2,
                       lty = "dashed") +
            geom_vline(xintercept = 
                           median(fit_1b[[input$v]]) - sd(fit_1b[[input$v]])*2,
                       lty = "dashed") +
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
                 caption = "Used the 'stan_glm' function.") +
            scale_y_continuous(labels = scales::percent_format())
    }, res = 96)
    
    output$plot4 <- renderPlot({
        ggplot(data = d, aes(d$`Nat. Defense Spending ($ in Mil.)`/10000, 
                             d$`Number of Treaties in the Year`)) +
            geom_point() +
            geom_smooth(color = "red", alpha = 0.2, 
                        method = "loess", formula = "y ~ x") +
            theme_bw() +
            labs(x = "Military Spending in Tens of Billions $",
                 y = "Number of Treaties Signed that Year")
    }, res = 96)
    
    output$plot5 <- renderPlot({
        new_obs <- tibble(`President Party` = input$a,
                          `Senate Party` = input$b,
                          `Military Spending` = input$c)
        
        pp <- posterior_predict(fit_1, newdata = new_obs) %>% 
            as_tibble() %>% 
            mutate_all(as.numeric)
        
        pp %>% 
            ggplot(aes(`1`)) +
            geom_histogram(aes(y = after_stat(count/sum(count))),
                           binwidth = 1,
                           color = "white",
                           fill = "palegreen3") +
            geom_vline(xintercept = mean(pp$`1`), color = "red") +
            geom_vline(xintercept = mean(pp$`1`) + sd(pp$`1`)*2, 
                       lty = "dashed") +
            geom_vline(xintercept = mean(pp$`1`) - sd(pp$`1`)*2, 
                       lty = "dashed") +
            theme_bw() +
            labs(title = "Posterior Probability Distributions",
                 subtitle = paste("For a", input$a, "presidency,", 
                                  "a", input$b, "Senate, and at $", 
                                  input$c*10, "billion"),
                 x = "Number of Treaties Signed",
                 y = "Probability") +
            coord_cartesian(xlim = c(0, 100), ylim = c(0, 0.2)) +
            scale_y_continuous(labels = scales::percent_format()) +
            scale_x_continuous(breaks = seq(0, 100, by = 5))
    }, res = 96)
    
    output$plot6 <- renderPlot({
        pp_check(fit_1, plotfun = "stat", stat = "mean", binwidth = 0.1)
    })
    
    output$plot7 <- renderPlot({
        pp_check(fit_1, plotfun = "dens_overlay")
    })
    
    output$plot8 <- renderPlot({
        pp_check(fit_2, plotfun = "stat", stat = "mean", binwidth = 0.1)
    })
    
    output$plot9 <- renderPlot({
        pp_check(fit_2, plotfun = "dens_overlay")
    })
    
    output$plot10 <- renderPlot({
        pp_check(fit_3, plotfun = "stat", stat = "mean", binwidth = 0.1)
    })
    
    output$plot11 <- renderPlot({
        pp_check(fit_3, plotfun = "dens_overlay")
    })
    
    output$plot12 <- renderPlot({
        pp_check(fit_4, plotfun = "stat", stat = "mean", binwidth = 0.1)
    })
    
    output$plot13 <- renderPlot({
        pp_check(fit_4, plotfun = "dens_overlay")
    })
    
    output$table1 <- render_gt(
        fit_1 %>% 
            tbl_regression() %>%
            as_gt() %>%
            tab_header(title = "Regression of Treaties Number (Negative Binomial)",
                       subtitle = "The impact of party and military spending 
                       1949-2020") %>%
            tab_source_note(md("Source: Congress.gov and WhiteHouse.gov")) %>% 
            tab_source_note(md("Distribution: Poisson"))
        )
    
    output$table2 <- render_gt(
        fit_4 %>% 
            tbl_regression() %>% 
            as_gt() %>% 
            tab_header(title = "Regression of Treaty Numbers: Presidents",
                       subtitle = "The impact of presidential uniqueness
                       alongside party and military spending") %>% 
            tab_source_note(md("Source: Congress.gov and WhiteHouse.gov")) %>% 
            tab_source_note(md("Distribution: Negative Binomial"))
        )
    
    output$table3 <- render_gt(
        fit_5 %>% 
            tbl_regression() %>% 
            as_gt() %>% 
            tab_header(title = "Treaties Number Regression: glm.nb",
                       subtitle = "The impact of the President's party, the 
                       Senate's party, and military spending, non-Bayesian
                       edition") %>% 
            tab_source_note(md("Source: Congress.gov and WhiteHouse.gov")) %>% 
            tab_source_note(md("Distribution: Negative Binomial"))
        )
    
    output$table4 <- render_gt(
        fit_6 %>% 
            tbl_regression() %>% 
            as_gt() %>% 
            tab_header(title = "President Dummy Regression: glm.nb",
                       subtitle = "The impact of presidential uniqueness
                       alongside party and military spending, non-Bayesian
                       edition") %>% 
            tab_source_note(md("Source: Congress.gov and WhiteHouse.gov")) %>% 
            tab_source_note(md("Distribution: Negative Binomial"))
        )
}

# Run the application 
shinyApp(ui = ui, server = server)
