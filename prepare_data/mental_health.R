
library(tidyverse)
theme_set(theme_light(base_size = 16))

# Read data sesjon IQ data
dat <- read_csv(here::here("data/iq.csv"))



psyc <- read_csv("N:/durable/Data14/DataModuler/kuhr/kuhr_psykisk_alle_2020-09-11.csv")

psyc <- psyc %>% select(BirthYear = FODT,lnr = LOPENR_k2_,diag = DIAGNOSER,year = DATO)

psyc <- psyc %>% 
  filter(lnr %in% dat$lnr)

psyc <- psyc %>% 
  separate_rows(diag) %>% 
  mutate(year = lubridate::year(year)) %>% 
  distinct()

psyc <- psyc %>% 
  filter(str_detect(diag,"P(7[0-9]|[89][0-9]|06|15|18|19)"))

psyc %>% count(diag)

psyc <- psyc %>% 
  mutate(year_diag = year-BirthYear)

psyc <- psyc %>% filter(between(year_diag,36,40))

psyc <- psyc %>% 
  select(-year,-year_diag) %>% 
  distinct()

psyc <- psyc %>% 
  mutate(value = 1L) %>% 
  arrange(diag) %>% 
  pivot_wider(values_from = value,names_from = diag,values_fill = 0L) %>% 
  mutate(anypsyc = 1L) %>% 
  relocate(anypsyc,.after = lnr) %>% 
  relocate(lnr,.before = BirthYear)

psyc <- left_join(dat %>% select(lnr,IQ,BirthYear = foedselsaar),psyc,by = c("lnr","BirthYear"))

psyc <- psyc %>% 
  mutate(across(.cols = where(is.integer),
                .fn = ~replace_na(.x,0L)))

write_csv(psyc,here::here("data","psyc.csv"))
