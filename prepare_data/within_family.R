
#within family iq
library(tidyverse)

dat <- read_csv(here::here("data","income.csv"))

# Exclude missing mother or father
dat <- dat %>% 
  filter(!is.na(mor_lnr) & !is.na(far_lnr))

dat <- dat %>% 
  mutate(fam_lnr = paste0(mor_lnr,far_lnr)) %>% 
  group_by(fam_lnr) %>% 
  mutate(n_fam = n()) %>% 
  ungroup() %>% 
  filter(n_fam > 1) %>% 
  arrange(fam_lnr)

dat %>% count(n_fam)

dat %>% 
  summarise(mom_miss = sum(is.na(mor_lnr)),
            dad_miss = sum(is.na(far_lnr)))

dat <- dat %>% 
  group_by(fam_lnr) %>% 
  mutate(famcenter_IQ = mean(IQ)) %>% 
  ungroup() %>% 
  mutate(within_IQ = IQ-famcenter_IQ) %>% 
  relocate(within_IQ,.after = IQ) %>% 
  relocate(famcenter_IQ,.after = within_IQ)

dat

write_csv(dat,here::here("data","withinIQ.csv"))
