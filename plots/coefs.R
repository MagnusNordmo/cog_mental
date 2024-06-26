# Coefficient Plot
library(tidyverse)
library(showtext)
theme_set(theme_minimal(base_size = 20))
font_add_google("Merriweather", "Merri", regular.wt = 400)
showtext_auto()

dat <- read_csv("models.csv")
within_dat <- read_csv("within_models.csv")

within_dat <- within_dat |> 
  mutate(model = "mod4")

dat <- bind_rows(dat,within_dat) %>% 
  arrange(name,model)

# Error spelling
dat <- dat %>% 
  mutate(name = case_when(name == "Depressiv disorder" ~ "Depressive disorder",
         T ~ name))

dat$name <- forcats::fct_reorder(dat$name,
                                 dat$estimate,
                                 .fun = min)

dat <- dat %>% 
  mutate(Category = case_when(name %in% c("Drug abuse","Medication abuse","Chronic alcohol abuse") ~ "Drug/Alcohol",
                              name %in% c("Schizophrenia","Affective psychosis","Psychosis","Personality disorder","Suicide attempt") ~ "Severe disorders",
                              name %in% c("Anxiety disorder","Phobia","Depressive disorder","PTSD","Sleep disturbance") ~ "Internalizing",
                              name %in% c("Mental disorder NOS","Hyperkinetic disorder") ~ "Other",
                              name == "Any mental disorder" ~ "Any mental disorder",
                              TRUE ~ "NA"))

dat$Category <- factor(dat$Category,levels = c("Any mental disorder",
                                               "Internalizing",
                                               "Drug/Alcohol",
                                               "Severe disorders",
                                               "Other"))

dat <- dat %>% 
  mutate(model = case_when(model == "mod1" ~ "Model 1: Cognitive ability predicting disorder",
                           model == "mod2" ~ "Model 2: Control for socioeconomic background",
                           model == "mod3" ~ "Model 3: Control for educational attainment",
                           model == "mod4" ~ "Model 4: Comparison of brothers"))


ggplot(dat,aes(estimate,name,xmin = conf.low,xmax = conf.high,
               color = model)) + 
  geom_point(position = position_dodge2(reverse = T,width = .9)) + 
  geom_errorbar(position = position_dodge2(reverse = T,width = 1)) + 
  geom_vline(xintercept = 1) +
  scale_x_continuous(sec.axis = sec_axis(~(1-.)*100,
                                         name = "Cognitive Abilities Estimate (Probability)")) + 
  ggthemes::scale_color_colorblind() + 
  ggforce::facet_col(~Category,scales = "free_y",space = "free") +
  labs(y = "Disorder",
       x = "Cognitive Abilities Estimate (Odds Ratio)",
       color = "Models",
       caption = "") + 
  theme(legend.position = "none")


ggsave("plots/coefs.pdf",width = 10,height = 16)
ggsave("plots/coefs.png",width = 5,height = 6,units = "in")


