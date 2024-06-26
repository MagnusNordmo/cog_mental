# Within sibling models
library(tidyverse)
library(lmtest)
library(sandwich)
library(jtools)

source(here::here("utils","get_coef_within.R"))
dat <- read_csv(here::here("data","withinIQ.csv"))

fit_glm_with_robust_se <- function(data) {
  a <- glm(value~within_IQ+edu_length,family = binomial(),data = data)
  
  
  broom::tidy(lmtest::coeftest(a,vcov. = sandwich::vcovCL(a,cluster = ~ fam_lnr)),
              conf.int = T)
}


cat("N Families: ",n_distinct(dat$fam_lnr),"\n",
    "Total N: ",n_distinct(dat$lnr),sep = "")

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
                                 "disability","Schi",
                                 "Dementia","Phobia","PTSD",
                                 "Suic"))) %>% 
  group_by(name) %>% 
  nest()

dat <- dat %>% 
  mutate(mod_coef = map(.x = data,
                     .f = ~fit_glm_with_robust_se(data = .x)))

dat <- dat %>% 
  mutate(mod_coef = map(.x = mod_coef,
                        .f = ~get_coefficients_within(.x,term = "within_IQ",
                                                      label = "mod1_")))

dat <- dat |> 
  unnest(mod_coef) |> 
  filter(mod1_term == "within_IQ") |> 
  select(-data)

#jtools::export_summs(dat$model,model.names = dat$name,
#                     to.file = "html",
#                     file.name = here::here("models","output","within_model1.html"),
#                     error_pos = "right")

dat <- dat |>  
  pivot_longer(cols = contains(c("estimate","conf")),
               names_sep = "_",
               names_to = c("model","b"))

dat <- dat |> 
  select(-model,-mod1_term) |> 
  pivot_wider(names_from = "b",
              values_from = "value")

# convert log-odds to odds-ratio
dat <- dat %>% 
  mutate(across(.cols = c(estimate,conf.high,conf.low),
                .fns = ~exp(.x)))

#write_csv(dat,here::here("data","within_models.csv"))
