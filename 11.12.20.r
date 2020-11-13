fit_2 <- stan_glm(data = d %>% 
                    mutate(spending = 
                             `Nat. Defense Spending ($ in Mil.)`/10000), 
                  `Number of Treaties in the Year` ~ 
                    spending + `President Party`*`Senate Party`,
                  refresh = 0,
                  family = poisson())

print(fit_2, 10)
-0.42 + (-0.27)*1 = -0.69
  
  fit_2b <- stan_glm(data = d %>% 
                      mutate(spending = 
                               `Nat. Defense Spending ($ in Mil.)`/10000), 
                    `Number of Treaties in the Year` ~ 
                      spending + `President Party`*`Senate Party` + `President`,
                    refresh = 0,
                    family = poisson())
