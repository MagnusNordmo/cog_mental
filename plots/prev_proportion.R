# Prevalence proportion plot
library(tidyverse)
library(patchwork)
library(showtext)

font_add_google("Merriweather", "Merri")
showtext_auto()

theme_set(theme_minimal(base_size = 16))



dat_plot <- read_csv(here::here("data","prev_prop.csv"))

# turn iq into factor
dat_plot <- dat_plot %>% 
  mutate(IQ = factor(IQ,levels = c(1,2,3,4,5,6,7,8,9)))

dat_plot$name <- forcats::fct_reorder(dat_plot$name,
                                      -dat_plot$ci_high,
                                      .fun = min)

panel_a <- dat_plot %>%
  ggplot(aes(IQ,prop,ymin = ci_low,ymax = ci_high)) + 
  geom_col() + 
  geom_errorbar(width = .6) +
  labs(title = "Panel A: Grouped by Cognitive Ability",
       y = "Proportion",
       x = "Cognitive Ability") +
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(~name,scales = "free_y") + 
  theme(plot.title.position = "plot")

panel_a

#Plot of achievement and all mental disorder prevalence
dat_plot <- read_csv(here::here("data","achievment_disorder_prev.csv"))

#Fix factors
dat_plot <- dat_plot %>% 
  mutate(edu_length2 = factor(edu_length2,
                              levels = c("Compulsory Education",
                                         "Upper Secondary",
                                         "Bachelors Degree",
                                         "Masters Degree")))


dat_plot$name <- factor(dat_plot$name,
                        levels = c("Any mental disorder",
                                   "Depressiv disorder",
                                   "Sleep disturbance",
                                   "Anxiety disorder",
                                   "Drug abuse",
                                   "Mental disorder NOS",
                                   "Hyperkinetic disorder",
                                   "Chronic alcohol abuse",
                                   "Phobia",
                                   "Schizophrenia",
                                   "Personality disorder",
                                   "Suicide attempt",
                                   "Medication abuse",
                                   "Psychosis",
                                   "Affective psychosis",
                                   "PTSD"))

panel_b <- dat_plot |> 
  ggplot(aes(edu_length2,prop,ymin = ci_low,ymax = ci_high)) + 
  geom_col(color = "black") + 
  geom_errorbar(width = .6) +
  facet_wrap(~name,scales = "free_y") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
  labs(title = "Panel B: Grouped by Educational Attainment",
       subtitle = "",
       x = "Educational attainment",
       y = "Proportion") + 
  theme(#axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1))


panel_b

ggsave(plot = panel_a,here::here("plots","prop_plotA.pdf"),height = 11,width = 15)
ggsave(plot = panel_b,here::here("plots","prop_plotB.pdf"),height = 11,width = 15)
