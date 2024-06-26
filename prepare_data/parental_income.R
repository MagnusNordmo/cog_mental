#Parental income

library(tidyverse)
income <- read_delim("N:/durable/Data14/original_data/k2_-w19_1011_8_nettoinnt_g1967g2018.txt",
                     delim = ";")
dat <- read_csv(here::here("data/edu_length.csv"))

income <- income %>% 
  rename(year = aargang,lnr = w19_1011_lnr_k2_)

income <- income %>% 
  filter(lnr %in% c(dat$mor_lnr,dat$far_lnr))

# Mom
mom_income <- left_join(dat %>% 
                          select(lnr,mor_lnr,BirthYear),income %>% 
                          rename(mor_lnr = lnr),
                        by = "mor_lnr")

mom_income <- mom_income %>% 
  filter(year == BirthYear)

mom_income %>% 
  summarise(na = sum(is.na(netto_inntekt)))

mom_income <- mom_income %>% 
  select(-BirthYear,-year) %>% 
  rename(mom_income = netto_inntekt)

dat <- left_join(dat,mom_income,by = c("lnr","mor_lnr"))

# Dad
dad_income <- left_join(dat %>% 
                          select(lnr,far_lnr,BirthYear),income %>% 
                          rename(far_lnr = lnr),
                        by = "far_lnr")

dad_income <- dad_income %>% 
  filter(year == BirthYear)

dad_income %>% 
  summarise(na = sum(is.na(netto_inntekt)))

dad_income <- dad_income %>% 
  select(-BirthYear,-year) %>% 
  rename(dad_income = netto_inntekt)

dat <- left_join(dat,dad_income,by = c("lnr","far_lnr"))

# How many NA?
dat %>% 
  summarise(across(.fns = ~sum(is.na(.x)))) %>% 
  pivot_longer(cols = everything()) %>% 
  arrange(-value)

#replace na income with median
dat %>% summarise(median_dad_income = median(dad_income,na.rm = T),
                  median_mom_income = median(mom_income,na.rm = T))

dat <- dat %>% 
  replace_na(list(mom_income = median(dat$mom_income,na.rm = T),
                  dad_income = median(dat$dad_income,na.rm = T)))

dat <- dat %>% 
  mutate(comb_income = mom_income+dad_income) 

hist(dat$comb_income)

dat <- dat %>% 
  mutate(comb_income = as.vector(scale(comb_income)))

dat <- dat %>% 
  mutate(comb_income = ntile(comb_income,99))

write_csv(dat,here::here("data/income.csv"))


