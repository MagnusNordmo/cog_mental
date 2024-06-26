# Main models
library(tidyverse)
library(jtools)

source(here::here("utils","get_coef.R"))
dat <- read_csv(here::here("data","income.csv"))
#dat <- dat %>% slice_sample(prop = .1) # for testing
#dat <- dat %>% select(lnr,IQ,comb_income,BirthYear,contains("edu"),anypsyc,P74,P76) # for testing
#dat <- dat %>% rename("Anxiety disorder" = "P74","Any disorder" = "anypsyc","Depressive disorder" = "P76")

#dat <- dat %>% select(lnr,IQ,comb_income,BirthYear,contains("edu"),anypsyc) # for testing
#dat <- dat %>% rename("Any mental disorder" = "anypsyc") # for testing

bigN <- n_distinct(dat$lnr)

dat <- dat %>% 
  rename("Any mental disorder" = "anypsyc",
         "Schizophrenia" = "P72",
         "Affective psychosis" = "P73",
         "Anxiety disorder" = "P74",
         "Depressiv disorder" = "P76",
         "Suicide attempt" = "P77",
         "Phobia" = "P79",
         "Personality disorder" = "P80",
         "Hyperkinetic disorder" = "P81",
         "PTSD" = "P82",
         "Psychosis" = "P98",
         "Mental disorder NOS" = "P99",
         "Sleep disturbance" = "P06",
         "Chronic alcohol abuse" = "P15",
         "Medication abuse" = "P18",
         "Drug abuse" = "P19")

dat <- dat %>% 
  pivot_longer(cols = contains(c("disorder","abuse","sleep","psycho",
                                 "PTSD",
                                 "disability","Schi","Phobia",
                                 "Suic"))) %>% 
  group_by(name) %>% 
  nest()

dat <- dat %>% 
  mutate(model = map(.x = data,
                     .f = ~glm(value~IQ,family = "binomial",data = .x)))



dat <- dat %>% 
  mutate(mod_coef = map(.x = model,
                        .f = ~broom::tidy(x = .x,conf.int = T)))

dat <- dat %>% 
  mutate(mod_coef = map(.x = mod_coef,
                        .f = ~get_coefficients(.x,
                                               label = "mod1_")))

dat <- dat %>% 
  unnest(mod_coef)

jtools::export_summs(dat$model,model.names = dat$name,
                     to.file = "html",
                     file.name = here::here("models","output","model1.html"),
                     error_pos = "right")

dat <- dat %>% 
  select(-model)

dat <- dat %>% 
  mutate(model2 = map(.x = data,
                     .f = ~glm(value~IQ+edu_length_mor+edu_length_far+comb_income,family = "binomial",data = .x)))

dat <- dat %>% 
  mutate(mod_coef2 = map(.x = model2,
                        .f = ~broom::tidy(x = .x,conf.int = T)))

dat <- dat %>% 
  mutate(mod_coef2 = map(.x = mod_coef2,
                        .f = ~get_coefficients(.x,
                                               label = "mod2_")))

jtools::export_summs(dat$model2,model.names = dat$name,
                     to.file = "html",
                     file.name = here::here("models","output","model2.html"),
                     error_pos = "right")

dat <- dat %>% 
  select(-model2) %>% 
  unnest(mod_coef2)


# Achieved education
dat <- dat %>% 
  mutate(model3 = map(.x = data,
                      .f = ~glm(value~IQ+edu_length_mor+edu_length_far+comb_income+edu_length,
                                family = "binomial",data = .x)))

dat <- dat %>% 
  mutate(mod_coef3 = map(.x = model3,
                         .f = ~broom::tidy(x = .x,conf.int = T)))

dat <- dat %>% 
  mutate(mod_coef3 = map(.x = mod_coef3,
                         .f = ~get_coefficients(.x,
                                                label = "mod3_")))

jtools::export_summs(dat$model3,model.names = dat$name,
                     to.file = "html",
                     file.name = here::here("models","output","model3.html"),
                     error_pos = "right")

dat <- dat %>% 
  select(-model3) %>% 
  unnest(mod_coef3)


dat <- dat %>% 
  pivot_longer(cols = contains(c("estimate","conf")),
               names_sep = "_",
               names_to = c("model","b"))

dat <- dat %>% 
  pivot_wider(names_from = "b",
              values_from = "value")

dat <- dat %>% 
  mutate(n = map_dbl(.x = data,
                     .f = ~sum(.x$value)))

# convert log-odds to odds-ratio
dat <- dat %>% 
  mutate(across(.cols = c(estimate,conf.high,conf.low),
                .fns = ~exp(.x)))

dat <- dat %>% 
  mutate(totalN = bigN) %>% 
  select(-data)

#write_csv(dat,here::here("data","models.csv"))
