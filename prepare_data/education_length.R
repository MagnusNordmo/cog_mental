
# IQ and length education
library(tidyverse)

# Read data sesjon IQ data
dat <- read_csv(here::here("data/psyc.csv"))
faste <- read_delim("N:/durable/Data14/original_data/k2_-w19_1011_4a_faste_oppl_ut.txt",delim = ";")

faste <- faste %>% select(lnr = w19_1011_lnr_k2_,mor_lnr = mor_lnr_k2_,far_lnr = far_lnr_k2_)

dat <- left_join(dat,faste,by = "lnr")

dat %>% 
  summarise(across(.cols = c(mor_lnr,far_lnr),
                   .fns = ~sum(is.na(.))))

# Create amount education by age 35
# Read Educational data
edu_raw <- read_delim("N:/durable/Data14/Rawdata/k2_-w19_1011_12b_f_utd_demografi_ut.TXT",delim = ";")

edu_length <- edu_raw %>% filter(w19_1011_lnr_k2_ %in% dat$lnr)
n_distinct(edu_length$w19_1011_lnr_k2_)

# Select only relevant columns, make BU into a single number
edu_length <- edu_length %>% 
  rename(lnr = w19_1011_lnr_k2_) %>% 
  mutate(BUDATO = as.integer(substr(BUDATO,1,4))) %>% 
  select(lnr,BUDATO,BU) %>% 
  mutate(BU = as.integer(substr(BU,1,1)))

edu_length

# Get foedselsdato 
edu_length <- left_join(dat %>% select(lnr,BirthYear),edu_length, by = "lnr")
n_distinct(edu_length$lnr)

# Assume that NA = Compulsory Education
edu_length %>% count(BU)
edu_length <- edu_length %>% replace_na(list(BU = 2))

edu_length %>% count(BU)

# Exclude education after 35 years old
edu_length <- edu_length %>% 
  mutate(f35 = BirthYear+35) %>% 
  filter(BUDATO <= f35)

n_distinct(edu_length$lnr)

# Get only highest edu
edu_length <- edu_length %>% 
  group_by(lnr) %>% 
  mutate(edu_length = max(BU)) %>% 
  ungroup() %>% 
  select(-BUDATO,-BU) %>% 
  distinct()

edu_length <- edu_length %>% select(-f35)

edu_length %>% count(edu_length)

# Get IQ
dat <- left_join(dat,edu_length,by = c("lnr","BirthYear")) %>% 
  relocate(edu_length,.after = BirthYear)



# Assume that NA & 1 = Compulsory Education
# edu length 1 is not possible
dat <- dat %>% 
  mutate(edu_length = case_when(is.na(edu_length) ~ 2,
                                edu_length == 1 ~ 2,
                                TRUE ~ edu_length))
dat %>% count(edu_length)

# Make simple education levels
dat <- dat %>% 
  mutate(edu_length2 =case_when(edu_length == 0 ~ "Compulsory Education",
                       edu_length == 1 ~ "Compulsory Education",
                       edu_length == 2 ~ "Compulsory Education",
                       edu_length == 3 ~ "Upper Secondary",
                       edu_length == 4 ~ "Upper Secondary",
                       edu_length == 5 ~ "Upper Secondary",
                       edu_length == 6 ~ "Bachelors Degree",
                       edu_length == 7 ~ "Masters Degree",
                       edu_length == 8 ~ "Masters Degree",
                       T ~ "Compulsory Education")) %>% 
  mutate(edu_length2 = factor(edu_length2,levels = c("Compulsory Education","Upper Secondary",
                                                     "Bachelors Degree","Masters Degree")))

# Mother education length
edu_length_mor <- edu_raw %>% filter(w19_1011_lnr_k2_ %in% dat$mor_lnr)
n_distinct(edu_length_mor$w19_1011_lnr_k2_)

# Select only relevant columns, make BU into a single number
edu_length_mor <- edu_length_mor %>% 
  rename(mor_lnr = w19_1011_lnr_k2_) %>% 
  mutate(BUDATO = as.integer(substr(BUDATO,1,4))) %>% 
  select(mor_lnr,BUDATO,BU) %>% 
  mutate(BU = as.integer(substr(BU,1,1)))

# Get only highest edu
edu_length_mor <- edu_length_mor %>% 
  group_by(mor_lnr) %>% 
  mutate(edu_length_mor = max(BU)) %>% 
  ungroup() %>% 
  select(-BUDATO,-BU) %>% 
  distinct()

edu_length_mor %>% count(edu_length_mor)

n_distinct(edu_length_mor$mor_lnr) == nrow(edu_length_mor)

dat

dat <- left_join(dat,edu_length_mor,by = "mor_lnr")

# Father education length
edu_length_far <- edu_raw %>% filter(w19_1011_lnr_k2_ %in% dat$far_lnr)
n_distinct(edu_length_far$w19_1011_lnr_k2_)

# Select only relevant columns, make BU into a single number
edu_length_far <- edu_length_far %>% 
  rename(far_lnr = w19_1011_lnr_k2_) %>% 
  mutate(BUDATO = as.integer(substr(BUDATO,1,4))) %>% 
  select(far_lnr,BUDATO,BU) %>% 
  mutate(BU = as.integer(substr(BU,1,1)))

# Get only highest edu
edu_length_far <- edu_length_far %>% 
  group_by(far_lnr) %>% 
  mutate(edu_length_far = max(BU)) %>% 
  ungroup() %>% 
  select(-BUDATO,-BU) %>% 
  distinct()

edu_length_far %>% count(edu_length_far)

n_distinct(edu_length_far$far_lnr) == nrow(edu_length_far)

dat

dat <- left_join(dat,edu_length_far,by = "far_lnr")

dat %>% 
  count(edu_length_far)

dat %>% 
  count(edu_length_mor)

# Make edu_length 1 into 2 mor
dat <- dat %>% 
  mutate(edu_length_mor = case_when(is.na(edu_length_mor) ~ 2L,
                                edu_length_mor == 1L ~ 2L,
                                TRUE ~ edu_length_mor))

# Make edu_length 1 into 2 far
dat <- dat %>% 
  mutate(edu_length_far = case_when(is.na(edu_length_far) ~ 2L,
                                    edu_length_far == 1L ~ 2L,
                                    TRUE ~ edu_length_far))
  

dat

write_csv(dat,here::here("data","edu_length.csv"))
