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

pp_check(fit_2_1, plotfun = "stat", stat = "mean")
pp_check(fit_2_1, plotfun = "dens_overlay")

fit_2_1 %>% 
  tbl_regression() %>%
  as_gt() %>% 
  tab_header(title = "Regression of Treaties Numbers",
             subtitle = "The impact of party and military spending 1949-2020") %>%
  tab_source_note(md("Source: Congress.gov and WhiteHouse.gov"))

fit_2_2 <- stan_glm(data = d %>% 
                      mutate(spending = 
                               `Nat. Defense Spending ($ in Mil.)`/10000), 
                    `Number of Treaties in the Year` ~ 
                      spending + `President Party`*`Senate Party`,
                    refresh = 0,
                    family = neg_binomial_2())

fit_2_2 %>% 
  tbl_regression() %>%
  as_gt() %>% 
  tab_header(title = "Regression of Treaties Numbers",
             subtitle = "The impact of party and military spending 1949-2020") %>%
  tab_source_note(md("Source: Congress.gov and WhiteHouse.gov"))

fit_3_1 %>% 
  tbl_regression() %>% 
  as_gt() %>% 
  tab_header("Regression of Treaty Numbers: Negative Binomial") %>% 
  tab_source_note(md("Source: Congress.gov and WhiteHouse.gov"))

pp_check(fit_2_3, plotfun = "stat", stat = "mean")
pp_check(fit_2_3, plotfun = "dens_overlay")

d2_test <- d %>% 
  mutate("Military Spending" = `Nat. Defense Spending ($ in Mil.)`/10000) %>%
  group_by(Year, `President Party`, `Senate Party`, `Military Spending`, 
           `Number of Treaties in the Year`, President) %>% 
  summarize("Number of Treaties" = mean(`Number of Treaties in the Year`),
            .groups = "drop") %>% 
  select(-`Number of Treaties in the Year`)

fit_2_1 <- stan_glm(data = d2_test, 
                    `Number of Treaties` ~ 
                      `Military Spending` + `President Party`*`Senate Party` +
                      President,
                    refresh = 0,
                    family = poisson())

fit_2_2 <- stan_glm(data = d2_test, 
                    `Number of Treaties` ~ 
                      `Military Spending` + `President Party`*`Senate Party`,
                    refresh = 0,
                    family = neg_binomial_2())

fit_2_3 <- stan_glm(data = d2_test, 
                    `Number of Treaties` ~ 
                      `Military Spending` + `President Party`*`Senate Party`,
                    refresh = 0,
                    family = poisson())

fit_3_1 <- glm.nb(data = d2_test,
                  `Number of Treaties` ~ 
                    `Military Spending` + 
                    `President Party`*`Senate Party` + President)

summary(fit_3_1)

fitted_fit_2_2 <- predict(fit_2_2) %>% 
  as_tibble() %>% 
  rename("fitted" = value) %>% 
  pull(fitted)

fitted_fit_2_3 <- predict(fit_2_3) %>% 
  as_tibble() %>% 
  rename("fitted" = value) %>% 
  pull(fitted)

d2_test %>% 
  ungroup() %>% 
  mutate(fitted = fitted_fit_2_2) %>% 
  rename(number = `Number of Treaties`) %>% 
  select(number, fitted) %>% 
  mutate(residuals = number - fitted) %>% 
  mutate(squares = residuals^2) %>% 
  summarize(mean = mean(squares)) %>% 
  summarize(RMSE = sqrt(mean))

d2_test %>% 
  ungroup() %>% 
  mutate(fitted = fitted_fit_2_3) %>% 
  rename(number = `Number of Treaties`) %>% 
  select(number, fitted) %>% 
  mutate(residuals = number - fitted) %>% 
  mutate(squares = residuals^2) %>% 
  summarize(mean = mean(squares)) %>% 
  summarize(RMSE = sqrt(mean))

library(tidymodels)

d_split <- initial_split(d, prob = 0.8)
d_train <- training(d_split)
d_test <- testing(d_split)

d_rec <- recipe(`Number of Treaties in the Year` ~ `President Party` + 
                  `Senate Party` + `Nat. Defense Spending ($ in Mil.)`, 
                data = d_train) %>% 
  step_dummy(all_nominal()) %>% 
  step_interact(~ `Senate Party`:`President Party`)

lm_model <-
  linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

lm_wflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(d_rec)

lm_fit <- fit(lm_wflow, d_train)

d_test_pred <- predict(lm_fit, new_data = d_test)

d_test_pred_2 <- bind_cols(d_test_pred, 
                           d_test %>% 
                             select(`Number of Treaties in the Year`))

rmse(d_test_pred_2, 
     truth = `Number of Treaties in the Year`,
     estimate = .pred)

fit_1 <- stan_glm(data = d2, 
                  `Number of Treaties` ~ 
                    `Military Spending` + `President Party`*`Senate Party`,
                  refresh = 0,
                  family = neg_binomial_2())

print(fit_1, 10)

ggplot(data = d2, aes(Year, `Number of Treaties`)) +
  geom_col()
