fit_2 <- stan_glm(data = d %>% 
                    mutate(spending = 
                             `Nat. Defense Spending ($ in Mil.)`/10000), 
                  `Number of Treaties in the Year` ~ 
                    spending + `President Party`*`Senate Party`,
                  refresh = 0,
                  family = poisson())

fit_2a <- fit_2 %>% 
  as_tibble()

print(fit_2, 10)