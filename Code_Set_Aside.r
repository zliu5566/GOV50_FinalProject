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