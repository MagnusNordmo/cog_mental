
#Plot of achievement and all mental disorder prevalence

# Prevalence proportion data
library(tidyverse)

dat <- read_csv(here::here("data","income.csv"))

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

dat_plot <- dat %>% 
  pivot_longer(cols = contains(c("disorder","abuse","sleep","psycho",
                                 "disability","Schi","PTSD","Phobia",
                                 "Suic"))) %>% 
  group_by(edu_length2,name) %>% 
  summarise(n = n(),
            sum = sum(value)) %>% 
  ungroup() %>% 
  mutate(prop = round(sum/n,6))

tmp <- DescTools::BinomCI(dat_plot$sum,dat_plot$n) %>% 
  as_tibble()

dat_plot <- dat_plot %>% 
  mutate(ci_high = tmp$upr.ci,
         ci_low = tmp$lwr.ci)

dat_plot <- dat_plot %>% 
  group_by(name) %>% 
  mutate(all_sum = sum(sum)) %>% 
  ungroup()

dat_plot


write_csv(dat_plot,here::here("data","achievment_disorder_prev.csv"))
