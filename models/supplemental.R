library(tidyverse)

dat <- readr::read_csv(here::here("data","income.csv"))
dat <- dat |>
  mutate(edu_length = case_when(
    edu_length == 0 ~ 1,
    edu_length == 1 ~ 1,
    edu_length == 2 ~ 1,
    edu_length == 3 ~ 2,
    edu_length == 4 ~ 2,
    edu_length == 5 ~ 2,
    edu_length == 6 ~ 3,
    edu_length == 7 ~ 4,
    edu_length == 8 ~ 4,
    TRUE ~ 1
  ))

# Is it linear or quadratic?
# Linear
mod_linear <- function(df) {
  glm(value ~ IQ, data = df, family = "binomial")
}

# Quadratic
mod_quadratic <- function(df) {
  glm(value ~ IQ + I(IQ^2), family = "binomial", data = df)
}

# Get p.value
get_p_value <- function(model1, model2) {
  anova_result <- anova(model1, model2, test = "Chisq")
  p_value <- anova_result$`Pr(>Chisq)`[2] # Extract the p-value for the model comparison
  return(p_value)
}

dat_out <- dat |>
  slice_sample(prop = 0.3) |> # for testing
  pivot_longer(cols = contains(c("anypsyc","P"), ignore.case = FALSE)) |>
  select(lnr, IQ, name, value) |>
  group_by(name) |>
  nest() |>
  mutate(mod_linear = map(data, mod_linear)) |>
  mutate(mod_quadratic = map(data, mod_quadratic)) |>
  select(name, mod_linear, mod_quadratic)

dat_AIC <- dat_out |>
  mutate(AIC = map2(mod_linear, mod_quadratic, AIC)) |>
  unnest(AIC) |>
  select(name, model = df, AIC) |>
  mutate(model = ifelse(model == 2, "linear", "quadratic"))

dat_BIC <- dat_out |>
  mutate(BIC = map2(mod_linear, mod_quadratic, BIC)) |>
  unnest(BIC) |>
  select(name, model = df, BIC) |>
  mutate(model = ifelse(model == 2, "linear", "quadratic"))

dat_out <- dat_out |>
  mutate(p_val = map2_dbl(mod_linear, mod_quadratic, get_p_value)) |>
  select(name, p_val)

dat_linquad <- full_join(dat_AIC, dat_BIC)
dat_linquad <- full_join(dat_linquad, dat_out)

dat_linquad <- dat_linquad |> mutate(across(where(is.numeric), ~round(.x, 3)))

dat_linquad <- dat_linquad |> 
  mutate(name = case_match(name,
                           "anypsyc" ~ "Any mental disorder",
                           "P72" ~ "Schizophrenia",
                           "P73" ~ "Psychosis",
                           "P74" ~ "Anxiety disorder",
                           "P76" ~ "Depressive disorder",
                           "P77" ~ "Suicide attempt",
                           "P79" ~ "Phobia",
                           "P81" ~ "Personality disorder",
                           "P82" ~ "Hyperkinetic disorder",
                           "P83" ~ "PTSD",
                           "P84" ~ "Psychosis",
                           "P85" ~ "Mental disorder NOS",
                           "P86" ~ "Sleep disturbance",
                           "P88" ~ "Chronic alcohol abuse",
                           "P89" ~ "Drug abuse",
                           "P91" ~ "Medication abuse",
                           .default = NA_character_))

# Testing for an interaction effect between cognitive abilities (1 to 9) and educational attainment (1 to 8) based on the International Standard Classification of Education (ISCED)
# 9 is unknown. This is caused by either lost data or immigration. We removed all individuals with a 9.
dat <- readr::read_csv(here::here("data", "income.csv"))
dat <- dat |> filter(edu_length != 9)

dat <- dat |> mutate(edu_length = edu_length - 2)

mod1 <- glm(anypsyc ~ IQ*edu_length + IQ*edu_length, data = dat)
sjPlot::sjt.glm(mod1, digits = 5)

# Make edu_length a factor
dat <- dat |> mutate(edu_length = factor(edu_length))
mod2 <- glm(anypsyc ~ IQ*edu_length + IQ*edu_length, data = dat)
summary(mod2)
sjPlot::sjt.glm(mod2, digits = 5)
sjPlot::plot_model(mod2, ci_method = "wald", type = "pred", terms = c("IQ", "edu_length"))

# Get predictions
new_data <- expand_grid(
  IQ = c(0:130),
  edu_length = factor(0:6))

# Generate predicted probabilities
predicted_probabilities <- predict(mod2, new_data, type = "response")
new_data <- new_data |> mutate(pred = predicted_probabilities)

ggplot(new_data, aes(x = IQ, color = factor(edu_length), y = pred)) +
  geom_line()
