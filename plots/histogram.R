
library(tidyverse)
library(patchwork)
library(showtext)

font_add_google("Merriweather", "Merri")
showtext_auto()

edu_dat <- read_csv(here::here("data","edu_dat.csv"))

n_diag <- read_csv(here::here("data/n_diag.csv"))

n_iq <- read_csv(here::here("data/n_iq.csv"))


theme_set(theme_minimal(base_size = 20))

# how large N for each stanine category?
a <- n_iq |> 
  ggplot(aes(factor(IQ),n)) + 
  geom_col() + 
  geom_text(aes(label = n),nudge_y = 2000) +
  labs(subtitle = "Panel A: Score Distribution",
       x = "Cognitive Abilities",
       y = "N") + 
  theme(plot.title.position = "plot") + 
  theme(panel.grid.major.x = element_blank())


a

# how many have each disorder?
b <- n_diag |> 
  ggplot(aes(prop,fct_reorder(name,prop))) + 
  geom_point(color = "black") + 
  geom_text(aes(label = scales::percent(prop,scale = 100,accuracy = .01)),
            nudge_x = .02,
            color = "black") + 
  geom_segment(aes(x = 0,xend = prop,y = name,yend = name),
               alpha = .2,
               lwd = 2) + 
  scale_x_continuous(limits = c(0,0.235),
                     breaks = seq(0,0.26,.04),
                     labels = scales::percent_format(accuracy = 1)) + 
  labs(subtitle = "Panel B: Five-Year Prevalence",
       x = "Percent",
       y = "Disorder") + 
  theme(axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        plot.title.position = "plot")

b

#Fix factors
edu_dat <- edu_dat %>% 
  mutate(edu_length = factor(edu_length,
                              levels = c("Compulsory Education",
                                         "Upper Secondary",
                                         "Bachelors Degree",
                                         "Masters Degree")))

# plot education length by IQ
c <- edu_dat |> 
  ggplot(aes(edu_length,prop_edulength)) + 
  geom_col(alpha = .8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(subtitle = "Panel C: Achieved Education by Cognitive Abilities (1-9)",
       x = "Education Level",
       y = "Proportion",
       fill = "") +
  facet_wrap(~IQ,ncol = 9) + 
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1),
        legend.position = "none",
        plot.title.position = "plot")


c

(a | b) / 
  c


ggsave(here::here("plots","descriptives.pdf"),
       height = 12,width = 17)
