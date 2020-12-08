fit_2a <- fit_2 %>% 
  as_tibble() %>%
  rename("pres_party_rep" = "`President Party`Republican") %>%
  rename("sen_party_rep" = "`Senate Party`Republican") %>%
  rename("interaction" = 
           "`President Party`Republican:`Senate Party`Republican")

output$plot3 <- renderPlot({
  ggplot(data = fit_2a, aes(spending)) +
    geom_histogram(aes(y = after_stat(count/sum(count))),
                   bins = 100,
                   color = "white") +
    theme_bw() +
    labs(title = "Posterior Distribution for the Coefficient of 'Military Spending'",
         subtitle = "'Military Spending': annual military spending in tens of billions $",
         x = "Coefficient of 'Military Spending'",
         y = "Probability",
         caption = "Used 'stan_glm' with the Poisson distribution.") +
    scale_y_continuous(labels = scales::percent_format())
}, res = 96)

output$plot5 <- renderPlot({
  ggplot(data = fit_2a, aes(fit_2a$`(Intercept)`)) +
    geom_histogram(aes(y = after_stat(count/sum(count))),
                   bins = 100,
                   color = "white") +
    theme_bw() +
    labs(title = "Posterior Distribution for the Intercept",
         subtitle = "Intercept: Democratic President and Senate",
         x = "Intercept",
         y = "Probability",
         caption = "Used 'stan_glm' with the Poisson distribution.") +
    scale_y_continuous(labels = scales::percent_format())
}, res = 96)

output$plot7 <- renderPlot({
  ggplot(data = fit_2a, aes(fit_2a$pres_party_rep)) +
    geom_histogram(aes(y = after_stat(count/sum(count))),
                   bins = 100,
                   color = "white") +
    theme_bw() +
    labs(title = "Posterior Distribution for the Republican President Coefficient",
         subtitle = "Coefficient if the President is Republican",
         x = "Coefficient of Republican President",
         y = "Probability",
         caption = "Used 'stan_glm' with the Poisson distribution.") +
    scale_y_continuous(labels = scales::percent_format())
}, res = 96)

output$plot8 <- renderPlot({
  ggplot(data = fit_2a, aes(fit_2a$sen_party_rep)) +
    geom_histogram(aes(y = after_stat(count/sum(count))),
                   bins = 100,
                   color = "white") +
    theme_bw() +
    labs(title = "Posterior Distribution for the Republican Senate Coefficient",
         subtitle = "Coefficient if the Senate is Republican",
         x = "Coefficient of Republican Senate",
         y = "Probability",
         caption = "Used 'stan_glm' with the Poisson distribution.") +
    scale_y_continuous(labels = scales::percent_format())
}, res = 96)

output$selected_var1 <- renderText({
  paste("Posterior Distribution for the Coefficient of", input$v)
})

output$selected_var2 <- renderText({
  paste("Coefficient of", input$v)
})

fit_3 %>% 
  tbl_regression() %>% 
  as_gt() %>% 
  tab_header("Regression of Treaty Numbers: Negative Binomial") %>% 
  tab_source_note(md("Source: Congress.gov and WhiteHouse.gov"))


tabPanel("Using the Model",
         h3("Predicting Treaty Numbers"),
         sidebarPanel(fluidPage(
           p("Set the following values to see my model's 
                              predictions for the number of treaties to be 
                                signed in such a year."),
           selectInput("a", "President Party", party),
           selectInput("b", "Senate Party", party),
           sliderInput("c", "Military Spending in Tens of 
                                          Billions $",
                       min = 0, max = 100, value = 50, 
                       round = FALSE)
         ),
         width = 4),
         mainPanel(fluidPage(
           plotOutput("plot5")
         ),
         width = 8))

fluidPage(
  splitLayout(
    cellWidths = c("50%", "50%"),
    plotOutput("plot6"),
    plotOutput("plot7")
  )
)

sidebarPanel(
  h4("Posterior Predictive Check: Mean"),
  fluidPage(
    plotOutput("plot6")
  ),
  width = 6
)

mainPanel(
  h4("Posterior Predictive Check: Distribution"),
  fluidPage(
    plotOutput("plot7")
  ),
  width = 6
)

fluidPage(
  selectInput("x", "X variable", columns),
  selectInput("y", "Y variable", columns),
  selectInput("geom", "geom", 
              c("point", "column", "jitter", 
                "bar"))
  )

column = 
  geom_col(aes(y = fct_reorder(.data[[input$y]], 
                               .data[["Year"]]),
               alpha = subset(d, !is.na(action_type)) %>% 
                 group_by(input$y) %>% 
                 mutate(number = n()) %>% 
                 pull(number)),
           position = "dodge")

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
          axis.text.y = element_text(size = 7.5),
          legend.position = "bottom") +
    labs(x = input$x,
         y = ifelse(input$geom == "bar", "Count", input$y))
}, res = 96)