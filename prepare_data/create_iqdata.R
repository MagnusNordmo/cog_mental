
# Create IQ sesjon data
library(tidyverse)
# Get basic ssb info
faste <- read_delim("N:/durable/Data14/original_data/k2_-w19_1011_4a_faste_oppl_ut.txt",delim = ";")

# Select only needed columns
faste <- faste %>% 
  select(lnr = w19_1011_lnr_k2_,foedselsaar,
         mor_lnr = mor_lnr_k2_,
         far_lnr = far_lnr_k2_,
         kjoenn)

faste


# Get intelligence from army data
defenceiq <- read_csv("N:/durable/Data14/dataNordmo/Generations/Data/defence_iq.csv")

defenceiq


dat <- left_join(defenceiq,faste,by = "lnr") %>% select(-mor_lnr,-far_lnr)

dat %>% count(foedselsaar)

# Include only 1970:1979 cohort
dat <- dat %>% filter(foedselsaar %in% 1970:1979)
dat |> count(ALDER_VED_SESJON)
dat |> 
  mutate(alder2 = fct_lump(factor(ALDER_VED_SESJON),6)) |> 
  count(alder2) |> 
  mutate(prop = n/sum(n))


rm(defenceiq)



dat %>% count(kjoenn) # A few women
dat %>% count(foedselsaar)

# Include only men
dat <- dat %>% filter(kjoenn == 1)
dat <- dat %>% select(-kjoenn)

n_distinct(dat$lnr)
n_distinct(dat$lnr) == nrow(dat)


# Make IQ into a factor
dat <- dat %>% mutate(IQ = factor(IQ,levels = c(1,2,3,4,5,6,7,8,9)))

n_distinct(dat$lnr)

#write_csv(dat,here::here("data/iq.csv"))
