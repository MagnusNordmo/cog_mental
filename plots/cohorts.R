
# Load necessary libraries 
library(tidyverse) # Create a data frame with all possible combinations of birth years and observation years 
birth_years <- seq(1970, 1979, 1) 

observation_years <- seq(2006, 2019, 1) 

data <- expand.grid(birth_year = birth_years, observation_year = observation_years) 

# Calculate the age of participants based on the birth year and observation year 
data$age <- data$observation_year - data$birth_year 
# Display the data frame 
print(data) 
# Create the plot 
ggplot(data, aes(x = birth_year, 
                             y = observation_year, 
                             fill = ifelse(age > 35 & age < 41,TRUE,FALSE))) + 
  geom_tile() + 
  geom_text(aes(label = age)) + 
  scale_x_continuous(name = "Birth Year", breaks = seq(1970, 1979, 1)) + 
  scale_y_continuous(name = "Available Primary Care Data", breaks = seq(2006, 2019, 1)) + 
  scale_fill_manual(values = c("white","#F0E442")) + 
  theme_classic(base_size = 16) + 
  theme(legend.position = "none")

ggsave("cohorts.pdf",height = 6,width = 8)


