
# Interaction plot
library(tidyverse)
library(showtext)

font_add_google("Merriweather", "Merri")
showtext_auto()

dat <- readr::read_csv(here::here("data","interaction.csv"))


theme_set(theme_minimal(base_size = 16))

dat$edu_length2 <- factor(dat$edu_length2,
                          levels = c("Compulsory Education",
                                     "Upper Secondary",
                                     "Bachelors Degree",
                                     "Masters Degree"))

dat |> 
  filter(below100 == FALSE) |> 
  ggplot(aes(IQ,prop,ymax = conf.high,ymin = conf.low,color = edu_length2)) + 
  geom_point() + 
  geom_line() +
  geom_errorbar(width = .3) +
  scale_y_continuous(labels = scales::percent_format(1),
                     limits = c(0,.4)) +
  scale_x_continuous(breaks = seq(1,9,1)) +
  ggthemes::scale_color_colorblind() + 
  labs(x = "Cognitive Abilities",
       y = "Proportion with Mental Disorder",
       color = "Educational Attainment")

ggsave(here::here("plots","interaction.pdf"),height = 6,width = 8)


