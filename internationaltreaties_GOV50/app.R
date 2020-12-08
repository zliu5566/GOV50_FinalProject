#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# 

# Libraries

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

# Main Dataset (Graphics)

d <- read_csv("treaties_data_Nov3.csv") %>% 
    
    # number = whether or not a year has a treaty document value
    
    mutate(number = map_dbl(document, ~ case_when(is.na(.) == TRUE ~ 0,
                                              TRUE ~ 1))) %>% 
    group_by(year) %>% 
    
    # number = number of treaties in that year
    
    mutate(number = sum(number)) %>% 
    
    # Two of the packages conflict, so I had to call "select" specifically
    
    dplyr::select(-title, -date, -document, -dollars) %>% 
    
    # To make the political parties easier to understand for the viewer
    
    mutate(pres_party = str_replace(pres_party, "D", "Democratic")) %>% 
    mutate(pres_party = str_replace(pres_party, "R", "Republican")) %>% 
    mutate(congress_party = str_replace(congress_party, "D", "Democratic")) %>% 
    mutate(congress_party = str_replace(congress_party, "R", "Republican")) %>%
    
    # To make sure that the action types don't count non treaty years
    
    mutate(action_type = na_if(action_type, "Not Applicable")) %>%
    
    # Reorganization of treaty topics and categories to prevent overcrowding
    
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
    
    # Makes Congress as factor to assist in ggplots down the road
    
    mutate(congress = factor(congress)) %>% 
    
    # Checks for partisan agreement between the Senate and the President
    
    mutate(pres_congress = 
               case_when(pres_party == "Democratic" & 
                             congress_party == "Democratic" ~ "Both Dem.",
                         pres_party == "Republican" & 
                             congress_party == "Republican" ~ "Both Rep.",
                         pres_party == "Democratic" & 
                             congress_party == "Republican" ~ 
                             "Dem. President \n Rep. Senate",
                         TRUE ~ "Rep. President \n Dem. Senate")) %>% 
    
    # Renames the columns for ease of interactivity down the road
    
    rename("Year" = year, "President Party" = pres_party, 
           "President" = president, "Congress" = congress, 
           "Senate Party" = congress_party, "Treaty Topic" = topic,
           "Senate Action" = senate_action, 
           "Nat. Defense Spending ($ in Mil.)" = new_dollars,
           "Number of Treaties in the Year" = number,
           "President and Senate Political Parties" = pres_congress)

# This is for graphic interactivity down the road

columns <- c("President", "Treaty Topic", "President Party", "Congress",
               "Senate Party", "President and Senate Political Parties")

# This is also for graphic interactivity down the road

party <- c("Democratic", "Republican")

# These are for overlaying political parties onto my treaties/spending graph

president_term <- tibble(start = c(1949, 1953, 1961, 1969, 1977, 1981, 1993,
                                   2001, 2009, 2017),
                         end = c(1953, 1961, 1969, 1977, 1981, 1993, 2001,
                                 2009, 2017, 2020),
                         base = rep(0, 10),
                         height = rep(40, 10),
                         party = c("D", "R", "D", "R", "D", "R", "D", "R", "D", 
                                   "R"))

senate_term <- tibble(start = c(1949, 1953, 1955, 1981, 1987, 1995, 2007, 2015),
                      end = c(1953, 1955, 1981, 1987, 1995, 2007, 2015, 2020),
                      base = rep(0, 8),
                      height = rep(40, 8),
                      party = c("D", "R", "D", "R", "D", "R", "D", "R"))

# Secondary Dataset (Models)

d2 <- d %>% 
    mutate("Military Spending" = `Nat. Defense Spending ($ in Mil.)`/10000) %>%
    group_by(Year, `President Party`, `Senate Party`, `Military Spending`, 
             `Number of Treaties in the Year`, President) %>% 
    summarize("Number of Treaties" = mean(`Number of Treaties in the Year`),
              .groups = "drop") %>% 
    select(-`Number of Treaties in the Year`)

# Model 1: Bayesian, Negative Binomial

fit_1 <- stan_glm(data = d2, 
                  `Number of Treaties` ~ 
                      `Military Spending` + `President Party`*`Senate Party`,
                  refresh = 0,
                  family = neg_binomial_2())

# Model 1 as tibbles for tables and graphs

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

# Model 2: Bayesian, Normal Distribution

fit_2 <- stan_glm(data = d2, 
                  `Number of Treaties` ~ 
                      `Military Spending` + `President Party`*`Senate Party`,
                  refresh = 0,
                  family = gaussian())

# Model 3: Bayesian, Poisson Distribution

fit_3 <- stan_glm(data = d2, 
                  `Number of Treaties` ~ 
                      `Military Spending` + `President Party`*`Senate Party`,
                  refresh = 0,
                  family = poisson())

# Model 4: Bayesian, Negative Binomial, President dummy

fit_4 <- stan_glm(data = d2, 
                  `Number of Treaties` ~ 
                      `Military Spending` + `President Party`*`Senate Party` +
                      President,
                  refresh = 0,
                  family = neg_binomial_2())

# Model 5: Non-Bayesian, Negative Binomial

fit_5 <- glm.nb(data = d2,
                `Number of Treaties` ~ 
                    `Military Spending` + `President Party`*`Senate Party`)

# Model 6: Non-Bayesian, Negative Binomial, President dummy

fit_6 <- glm.nb(data = d2,
                `Number of Treaties` ~ 
                    `Military Spending` + `President Party`*`Senate Party` +
                    President)

# User Interface

ui <- navbarPage(
    
    # It was annoying trying to find a theme that fit.
    
    theme = shinytheme("lumen"),
    
    # Clever title, eh? Sounds like a zoo before it gets boring.
    
    title = paste("Elephants, Donkeys, Doves, and Hawks:
                  Predicting U.S. Treaties"),
    
    # First/Intro panel
    
    tabPanel("Predicting Treaties", 
             titlePanel("Predicting U.S. International Treaties"),
             
             # The star graphic of the show
             
             sidebarPanel(
                 fluidPage(
                     p("Set the following values to predict the number of 
                        treaties that will be signed in such a year."),
                     selectInput("a", "President Party", party),
                     selectInput("b", "Senate Party", party),
                     sliderInput("c", "Military Spending in Tens of Billions $",
                                 min = 0, max = 100, value = 50, round = FALSE)
                     ),
                 
                 # After much experimentation, the 4/8 divide between the side 
                 # and main panels looks he best for this graph
                 
                 width = 4),
             mainPanel(
                 fluidPage(
                     plotOutput("plot5")
                     ),
                 width = 8),
             
             # Description of the project
             
             h3("Project Overview"),
             p("Treaties are one of the most important facets of U.S. foreign
               policy; as a result, the power to determine whether or not this 
               country enters into one is split between the executive and 
               legislative branches of government. As per Article II, section 2
               of the U.S. Constitution, the President of the United States 
               'shall have Power, by and with the Advice and Consent of the 
               Senate, to make Treaties, provided two thirds of the Senators 
               present concur.' In practical terms, the Treaty Claus grants the 
               President the authority to negotiate and sign international 
               treaties, which must then be ratified by a supermajority vote of 
               the Senate in order to take effect as law on American soil."),
             p("This project looks at U.S. international treaties from 1949 to 
               2020. Specifically, it attempts to determine the factors that 
               may impact the number of treaties the U.S. Senate receives from 
               the President in any given year and then tries to use that 
               information to build a model predicting the number of treaties 
               given a set of parameters. I decided to set the timeframe of 
               the treaties included in this project to be from 1949 to 2020
               because I feel that only treaties in this time period are 
               'modern' enough to be useful predictors. 1949 was the year in 
               which the Cold War became 'official,' with the Soviet Union
               developing the atom bomb, China becoming the People's Republic
               of China, and the Western powers signing the North Atlantic
               Treaty (the foundation of NATO). Only then did the world begin 
               to develop our modern understanding of treaties and their 
               importance to global stability. Any treaties signed before 1949 
               were either directly tied to the World Wars (two anomolous events 
               that are unlikely to be good predictors) or be representative of 
               too antiquated a geopolitical outlook (e.g. colonialism). Hence, 
               1949, I believe, is a fine turning point for modern treaties."),
             ),
    
    # Second/Data Visualization Panel
    
    tabPanel("Graphics",
             
             # These are mostly just cool graphics and interactions I put 
             # together to help viewers better understand my data.
             
             tabsetPanel(
                 tabPanel("Visualizing the Data",
                          titlePanel("Visualizing Data Correlations"),
                          h3("Military Spending vs. Treaties Signed: 
                             1949-2020"),
                          sidebarPanel(
                              fluidPage(
                                  p("Select either President or Senate in order
                                    to see which parties controlled the branch
                                    from 1949-2020."),
                                  selectInput("branch", "Party of the:", 
                                              c("President", "Senate"))
                                  ),
                              
                              # Since the text is shorter in this panel and
                              # the graph is wider, 3/9 is the best division
                              
                              width = 3
                              ),
                          mainPanel(
                              fluidPage(
                                  plotOutput("plot1")
                                  ),
                              width = 9
                              ),
                          
                          # This graphic is a more explicit version of the 
                          # previous one as I was figuring out my model. It 
                          # looks cool though, so I kept it.
                          
                          h3("Correlation Between Spending and Treaties"),
                          fluidPage(
                              plotOutput("plot4")
                              )),
                 
                 # Honestly, I am still not quite sure what this graphic
                 # depicts, but the code for this was absurdly painful to
                 # write and it is visually spectacular, so here we go.
                 
                 tabPanel("Exploring the Data", 
                          titlePanel("Pick Your Own Data Visualization"),
                          sidebarPanel(
                              fluidPage(
                                  selectInput("x", "X variable", columns),
                                  selectInput("y", "Y variable", columns),
                                  selectInput("geom", "geom", 
                                              c("point", "jitter", "bar"))
                                  ),
                              width = 3
                              ),
                          mainPanel(fluidPage(
                              plotOutput("plot2")
                              ),
                              width = 9)
                          ))
             ),
    
    # Third/Statistical Model Panel
    
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
                          
                          # Posterior distributions of coefficients
                          
                          h3("Creating a Linear Model"),
                          p("The formula upon which I settled for this model 
                            includes: an intercept, military spending (in tens 
                            of billions of dollars, since the military spends 
                            too much to calculate the impact of individual 
                            dollars being spent), the party of the president, 
                            the party of the Senate majority, and an interaction
                            variable to account for the effects of having a 
                            president and a Senate majority being of the same 
                            party. These are the only factors that I believe 
                            will hold steady enough over the next few years 
                            to allow for prediction (since Eisenhower is likely 
                            not coming back from the dead, and it is highly 
                            improbably that we will suddenly be back in 1973
                            sometime in the future)."),
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
                 
                 # Interpretation and first regression table
                 
                 tabPanel("Explaining the Model",
                          h3("Interpreting the Numbers"),
                          p("The regression table of the model I built can be 
                            seen below:"),
                          fluidPage(
                              gt_output("table1")
                              ),
                          h4("Military Spending"),
                          p("What we see here is that contrary to expectations, 
                            military spending is, for all intents and purposes, 
                            almost completely insignificant for predicting the 
                            number of treaties in a year. That is probably the 
                            result of few treaties being signed and fairly low 
                            military spending during the Truman and Eisenhower 
                            years, as we saw in the Graphics section. However,
                            I do still believe that this is important to include 
                            in the model, especially since I think that when the 
                            dataset grows over the next few years, military 
                            spending is likely to become quite significant."),
                          h4("President Party"),
                          p("It seems that the political party of the president
                            is almost significant, with a slight increase in the 
                            number of treaties likely to be passed on to the 
                            Senate if the President is Republican with a 
                            Democratic Senate compared to the Democrats 
                            controlling both branches (the intercept value). 
                            However, since 0 is still within our 95% confidence 
                            interval, the political party of the President may 
                            still have no real impact."),
                          h4("Senate Party"),
                          p("The majority party of the Senate is indeed 
                            significant: a Republican led Senate with a 
                            Democratic President is very likely to recieve more 
                            treaties than the incercept default. This, coupled 
                            with the probably positive impact of a Republican 
                            President with a Democratic Senate, suggests that 
                            a split Senate/President is likely to have more 
                            treaties signed. I think that the reasons for this 
                            may be twofold: (1) a President that cannot have
                            legislation passed in the Senate for partisan 
                            reasons is unlikely to score domestic 'victories' 
                            and so may look to the foreign policy arena (where 
                            it is more difficult for the Senate to intervene)
                            for accomplishments and good press, and (2) since 
                            a Senate of different parties with the President is 
                            unlikely to ratify treaties, the President may be 
                            more inclined to sign treaties that sound great on 
                            paper (and so score more political points with the 
                            public) but would be difficult/detrimental to 
                            enforce on American soil knowing that the treaty
                            is unlikely to become law anyways."),
                          h4("Interaction Term"),
                          p("The interaction term for if both the Senate and the 
                            President are Republican is very nearly significant
                            and almost perfectly cancels out the added median 
                            beta terms for individually Republican White Houses 
                            or Senates. This further proves my theory explicated 
                            above about split Senate/Presidents being more 
                            likely to sign treaties.")
                          ),
                 
                 # pp_checks for various distribution families in the model
                 
                 tabPanel("Selecting a Distribution",
                          p("When I was writing my model, I had some trouble 
                            figuring out which distribution family I should 
                            place in my stan_glm function. Here is the process 
                            through which I went to finally arrive at a 
                            negative binomial distribution. In each of the 
                            instances below, the formulae are all the same; only 
                            the 'families' are different."),
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
                          p("This proved to be the best distribution family for 
                            my model, mostly because it behaved better than 
                            either the Gaussian (normal) or Poisson families. As 
                            we see here, running posterior predictive checks on 
                            my model with the negative binomial distribution
                            showed that it could fairly accurately predict the 
                            mean of the data and its spread. In addition, the 
                            model is quite good at matching data with discrete 
                            events (like tornado outbreaks), which is just fine 
                            and dandy with my data. The biggest problem with 
                            this distribution is that its variability and hence 
                            dispersion is way to big (as can be seen by the 
                            massive tail in the Distribution graph)."),
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
                          p("At first glance, the Gaussian/Normal distribution 
                            looks like it would be the best: the mean prediction 
                            appears to line up nicely with reality, and the 
                            distribution match is almost perfect. However, this 
                            distribution is ultimately unsuitable for my data, 
                            precisely because of the distribution. It is in the 
                            nature of the Normal to have the mean also be the 
                            mode and therefore go equally in both directions 
                            from the 'peak' of the distribution. In the case of 
                            my data, however, the 'peak' is 0 or a value that is 
                            very quite close to 0, which forces the 
                            distribution to go negative, which makes no sense 
                            at all in terms of treaties, since negative treaties 
                            do not exist. Honestly, the best distribution for my 
                            data would probably have been a folded normal, but 
                            since I could not for the life of my figure out how 
                            to work that into a stan_glm, I went with the 
                            negative binomial."),
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
                          p("The Poisson distribution initially looked very 
                            attractive to me because it has a tightly controlled 
                            spread, resulting in neater looking graphs with 
                            narrower 95% confidence intervals in posterior 
                            predictions. In addition, the Mean check for a 
                            Poisson based model looks spectacularly accurate. 
                            The biggest problem with the Poisson, though, as 
                            you may very well have noticed, is in the 
                            distribution. The Poisson holds that the mean and 
                            variance of a dataset should be equal, which is 
                            far from reality in the case of my data; indeed, my 
                            data seems to fall into the category of greatest 
                            weakness for the Poisson: associated discrete 
                            events. And, finally, Poisson's greatest strength is 
                            with small observation number data, which is not 
                            the case at all with my data.")
                          ),
                 
                 # Dummy variables: Presidential uniqueness
                 
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
                          p("What we see here is fascinating: when one takes 
                            into account the existence of presidential 
                            uniqueness that cannot be explained by party, the 
                            ability of the president's party to explain 
                            treaty numbers drops to being pretty much
                            insignificant. This does make sense: the Republican
                            party went through some very significant changes 
                            during the time of my dataset as it established 
                            itself under Reagan as the party of social 
                            conservatism; this means that presidential 
                            uniqueness becomes a much more important factor and 
                            shows that Presidents tend not to be complete 
                            party tools. We also see that the interaction
                            term drops in significance as well, probably 
                            because of the drop in the President Party term. 
                            Even more interesting is that the Senate Party, 
                            although dropping somewhat, is still almost 
                            significant even taking into account Presidential 
                            uniqueness. This strongly suggests that my theory 
                            about Senate/President split is at least somewhat 
                            accurate. Military spending remained the same."),
                          splitLayout(
                              cellWidths = c("55%", "45%"),
                              h4("Posterior Predictive Check: Mean"),
                              h4("Posterior Predictive Check: Distribution")
                              ),
                          
                          # It took me so long to figure out how to make the 
                          # best side by side for these two graphs.
                          
                          fluidPage(
                              splitLayout(
                                  cellWidths = c("55%", "45%"),
                                  plotOutput("plot12"),
                                  plotOutput("plot13")
                                  )
                              ),
                          p("Shockingly, adding more variables to a 
                            model makes it fit better with the data. The 
                            negative binomial distribution looks a lot better
                            with presidential uniqueness accounted for, since 
                            the tail is not nearly as crazy long as previously.
                            The mean check also looks very good.")
                          ),
                 
                 # glm.nb models
                 
                 tabPanel("A Non-Bayesian Approach",
                          h3("Using glm.nb"),
                          p("For the sake of thoroughness, I also decided to 
                            create a non-Bayesian model for my data using the 
                            glm.nb function, the regression table of which is 
                            below:"),
                          fluidPage(
                              gt_output("table3")
                              ),
                          p(" "),
                          p("By and large, this model agrees with my Bayesian 
                            model: military spending isn't significant at all, 
                            just being a Republican President isn't significant, 
                            the Senate being Republican is significant, and 
                            the interaction term is also significant. What is 
                            nice about these frequentist models, though, 
                            compared to Bayesian is that they are much quicker 
                            to run and give p-values in R, which makes it that 
                            much easier to interpret."),
                          h3("Dummy Variable (Part II)"),
                          p("Here is the glm.nb model with the President dummy
                            variable:"),
                          fluidPage(
                              gt_output("table4")
                              ),
                          p("Things get interesting with the President dummy
                            variable. While military spending remains quite 
                            insignificant, the party of the President as a 
                            Republican jumps to being significant (even more so
                            than the Senate being Republican), which is a 
                            huge departure from the Bayesian model. Meanwhile,
                            the interaction term becomes insignificant, which is 
                            very different as well. Fortunately, the Senate 
                            variable remains significant, which is reassuring. 
                            Why are there these differences? I do not know, 
                            since I am not a statistician. What I do know,
                            however, is that since this is a frequentist model 
                            that has p-values, I can tell you which Presidents 
                            are very, very significant in reverse alphabetical
                            order: Trump, Truman, Obama, Kennedy, Johnson, 
                            and Eisenhower. Who would have guessed?")
                          )
                 ),
             ),
    
    # Fourt/About panel
    
    tabPanel("About",
             titlePanel("About this Project"),
             h3("Data Collection"),
             p("I collected this data by scraping Congress.gov, which has the 
               voting records of the U.S. Senate for every treaty that was 
               submitted to that body since 1949, which is perfect for my 
               project anyways re the date. I then added to that dataset
               information about presidential terms (which is common knowledge) 
               and the dominant political party of the Senate for each Congress, 
               which I got from Senate.gov. Finally, I added to this 
               dataset national security spending data from 1949 to the present,
               which I acquired from Whitehouse.gov as an Excel, then converted
               to a CSV before wrangling it with the rest. I then used the 
               package PriceR to adjust for inflation on military spending."),
             h3("About the Author"),
             p(HTML(paste0(
               "My name is Z. Liu, and I study many things. You can reach me 
               when this project is done. In the meantime, please enjoy my
               GitHub repository for this project ", 
               a(href = 'https://github.com/zliu5566/US_Int.Treaties_Mil.Spending',
                 "here"), "."))))
    )

# Server Logic

server <- function(input, output, session) {
    
    # For my Graphics tab
    
    plot_geom <- reactive({
        switch(input$geom,
               point = geom_point(aes(y = fct_reorder(.data[[input$y]],
                                                      .data[["Year"]]))),
               
               jitter = geom_jitter(aes(y = fct_reorder(.data[[input$y]], 
                                                        .data[["Year"]])), 
                                    width = 0.2, 
                                    height = 0.2, alpha = 0.5),
               bar = geom_bar()
               )
        })
    
    # For a different graphic in the Graphics tab
    
    data_input <- reactive({
        switch(input$branch,
               President = president_term,
               Senate = senate_term
               )
        })
    
    # Treaties, Spending, Parties, and Time

    output$plot1 <- renderPlot({
        ggplot() +
            geom_rect(data = data_input(),
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
            scale_fill_manual(name = paste(input$branch, "Party"),
                              labels = c("Democratic", "Red"),
                              values = c("blue", "red")) +
            theme_bw() +
            theme(legend.position = "bottom") +
            scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
            labs(x = "Year")
    }, res = 96)
    
    # Strange interactive graphic
    
    output$plot2 <- renderPlot({
        ggplot(data = subset(d, !is.na(action_type)), 
               aes(fct_reorder(.data[[input$x]], .data[["Year"]]),
                   color = action_type, fill = action_type)) +
            plot_geom() +
            scale_fill_discrete(name = "Senate Action Taken") +
            scale_color_discrete(name = "Senate Action Taken") +
            theme_bw() +
            theme(axis.text.x = element_text(size = 7.5, angle = -90),
                  axis.text.y = element_text(size = 7.5),
                  legend.position = "bottom") +
            labs(x = input$x,
                 y = ifelse(input$geom == "bar", "Count", input$y))
        }, res = 96)
    
    # Posterior distribution of coefficients
    
    output$plot3 <- renderPlot({
        ggplot(data = fit_1b, aes(.data[[input$v]])) +
            geom_histogram(aes(y = after_stat(count/sum(count))),
                           bins = 100,
                           color = "white",
                           fill = "deepskyblue3") +
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
    
    # Treaties and spending
    
    output$plot4 <- renderPlot({
        ggplot(data = d, aes(d$`Nat. Defense Spending ($ in Mil.)`/10000, 
                             d$`Number of Treaties in the Year`)) +
            geom_point() +
            geom_smooth(color = "red", alpha = 0.2, 
                        method = "loess", formula = "y ~ x") +
            theme_bw() +
            labs(x = "Military Spending in Tens of Billions $",
                 y = "Number of Treaties Signed that Year") +
            scale_y_continuous(breaks = seq(0, 45, by = 5))
        }, res = 96)
    
    # The crown jewel predictor
    
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
    
    # Model 1 pp_check mean
    
    output$plot6 <- renderPlot({
        pp_check(fit_1, plotfun = "stat", stat = "mean", binwidth = 0.1)
        })
    
    # Model 1 pp_check distribution
    
    output$plot7 <- renderPlot({
        pp_check(fit_1, plotfun = "dens_overlay")
        })
    
    # Model 2 pp_check mean
    
    output$plot8 <- renderPlot({
        pp_check(fit_2, plotfun = "stat", stat = "mean", binwidth = 0.1)
        })
    
    # Model 2 pp_check distribution
    
    output$plot9 <- renderPlot({
        pp_check(fit_2, plotfun = "dens_overlay")
        })
    
    # Model 3 pp_check mean
    
    output$plot10 <- renderPlot({
        pp_check(fit_3, plotfun = "stat", stat = "mean", binwidth = 0.1)
        })
    
    # Model 3 pp_check distribution
    
    output$plot11 <- renderPlot({
        pp_check(fit_3, plotfun = "dens_overlay")
        })
    
    # Model 4 pp_check mean
    
    output$plot12 <- renderPlot({
        pp_check(fit_4, plotfun = "stat", stat = "mean", binwidth = 0.1)
        })
    
    # Model 4 pp_check distribution
    
    output$plot13 <- renderPlot({
        pp_check(fit_4, plotfun = "dens_overlay")
        })
    
    # Table for Model 1
    
    output$table1 <- render_gt(
        fit_1 %>% 
            tbl_regression() %>%
            as_gt() %>%
            tab_header(title = "Regression of Treaties Number: Main Model",
                       subtitle = "The impact of party and military spending 
                       1949-2020") %>%
            tab_source_note(md("Source: Congress.gov and WhiteHouse.gov")) %>% 
            tab_source_note(md("Distribution: Negative Binomial"))
        )
    
    # Table for Model 4
    
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
    
    # Table for Model 5
    
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
    
    # Table for Model 6
    
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
