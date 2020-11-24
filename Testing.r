fit_2_1 <- stan_glm(data = d %>% 
                    mutate(spending = 
                             `Nat. Defense Spending ($ in Mil.)`/10000), 
                  `Number of Treaties in the Year` ~ 
                    spending + `President Party`*`Senate Party`,
                  refresh = 0,
                  family = gaussian())

new_obs_1 <- tibble(`President Party` = "Democratic",
                  `Senate Party` = "Republican",
                  spending = 50)

pp <- posterior_predict(fit_2_1, newdata = new_obs_1) %>% 
  as_tibble() %>% 
  mutate_all(as.double)

pp %>% 
  ggplot(aes(`1`)) +
  geom_histogram(aes(y = after_stat(count/sum(count))),
                     bins = 100) +
  theme_bw()