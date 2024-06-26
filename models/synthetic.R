# Synthetic data simulation to run regression models

# This will not reproduce our results due to the coarse nature of the simulation

library(tidyverse)

# custom function to extract cofficients
get_coefficients <- function(.x,label) {
  .x <- filter(.x,term == "IQ")
  
  .x <- select(.x,-std.error,-statistic,-p.value,-term)
  
  .x <- rename_with(.x,~str_c(label,.))
  
  .x
}


cov <- read_csv(here::here("data/sim_cov.csv")) |> as.data.frame()

means <- read_csv(here::here("data/sim_means.csv")) |> as_vector()

# Simulate synthetic data based on means and covariances. Keep in mind that the models take several minutes
# (perhaps hours) to run on a strong machine with large N

# Tip: Start with a low N to test
sdat <- MASS::mvrnorm(n = 200000,mu = means,Sigma = cov)

sdat <- as_tibble(sdat)

sdat <- sdat |> 
  mutate(across(.cols = c(edu_length,IQ,edu_length_far,edu_length_mor),
                ceiling))

# make diagnosis either 0 or 1
sdat <- sdat |> 
  mutate(across(.cols = contains(c("P","anypsyc"),ignore.case = F),
                ceiling))

# remove 2s and -1
sdat <- sdat |> 
  mutate(across(.cols = contains(c("P","anypsyc"),ignore.case = F),
                ~ifelse(.x > 1,1,.x)))

sdat <- sdat |> 
  mutate(across(.cols = contains(c("P","anypsyc"),ignore.case = F),
                ~ifelse(.x < 0,0,.x)))

sdat |> count(anypsyc)

sdat <- sdat %>% 
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

sdat <- sdat %>% 
  pivot_longer(cols = contains(c("disorder","abuse","sleep","psycho",
                                 "PTSD",
                                 "disability","Schi","Phobia",
                                 "Suic"))) %>% 
  group_by(name) %>% 
  nest()

sdat <- sdat %>% 
  mutate(model = map(.x = data,
                     .f = ~glm(value~IQ,family = "binomial",data = .x)))



sdat <- sdat %>% 
  mutate(mod_coef = map(.x = model,
                        .f = ~broom::tidy(x = .x,conf.int = T)))

sdat <- sdat %>% 
  mutate(mod_coef = map(.x = mod_coef,
                        .f = ~get_coefficients(.x,
                                               label = "mod1_")))

sdat <- sdat %>% 
  unnest(mod_coef)


sdat <- sdat %>% 
  select(-model)

sdat <- sdat %>% 
  mutate(model2 = map(.x = data,
                      .f = ~glm(value~IQ+edu_length_mor+edu_length_far+comb_income,family = "binomial",data = .x)))

sdat <- sdat %>% 
  mutate(mod_coef2 = map(.x = model2,
                         .f = ~broom::tidy(x = .x,conf.int = T)))

sdat <- sdat %>% 
  mutate(mod_coef2 = map(.x = mod_coef2,
                         .f = ~get_coefficients(.x,
                                                label = "mod2_")))

sdat <- sdat %>% 
  select(-model2) %>% 
  unnest(mod_coef2)


# Achieved education
sdat <- sdat %>% 
  mutate(model3 = map(.x = data,
                      .f = ~glm(value~IQ+edu_length_mor+edu_length_far+comb_income+edu_length,
                                family = "binomial",data = .x)))

sdat <- sdat %>% 
  mutate(mod_coef3 = map(.x = model3,
                         .f = ~broom::tidy(x = .x,conf.int = T)))

sdat <- sdat %>% 
  mutate(mod_coef3 = map(.x = mod_coef3,
                         .f = ~get_coefficients(.x,
                                                label = "mod3_")))

sdat <- sdat %>% 
  select(-model3) %>% 
  unnest(mod_coef3)


sdat <- sdat %>% 
  pivot_longer(cols = contains(c("estimate","conf")),
               names_sep = "_",
               names_to = c("model","b"))

sdat <- sdat %>% 
  pivot_wider(names_from = "b",
              values_from = "value")

sdat <- sdat %>% 
  mutate(n = map_dbl(.x = data,
                     .f = ~sum(.x$value)))

# convert log-odds to odds-ratio
sdat <- sdat %>% 
  mutate(across(.cols = c(estimate,conf.high,conf.low),
                .fns = ~exp(.x)))

sdat <- sdat %>% 
  select(-data)

sdat

#write_csv(sdat,here::here("data","models.csv"))

