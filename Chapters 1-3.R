print("Hello world!")
print("eep")

# Chapter 3 - Modeling Review ---------------------------------------------


# 3.1 - An Example --------------------------------------------------------

library(tidyverse)
data(crickets, package = "modeldata")
names(crickets)

ggplot(crickets,
       aes(x = temp, y = rate, color = species, pch = species, lty = species)) +
  geom_point(size = 2) + 
  geom_smooth(method = lm, se = FALSE, alpha = 0.5) +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Temperature (C)", y = "Chirp Rate (per minute)")

interaction_fit <- lm(rate ~ (temp + species)^2, data = crickets )
interaction_fit

par(mfrow = c(1,2))
plot(interaction_fit, which = 1)
plot(interaction_fit, which = 2)

main_effect_fit <- lm(rate ~ temp + species, data = crickets)

anova(main_effect_fit, interaction_fit)

summary(main_effect_fit)

new_values <- data.frame(species = "O. exclamationis", temp = 15:20)
predict(main_effect_fit, new_values)


# 3.4- Combining Base R Models and the Tidyverse ----------------------------

split_by_species <-
  crickets |>
  group_nest(species)

split_by_species

model_by_species <-
  split_by_species |>
  mutate(model = map(data, ~ lm(rate ~ temp, data = .x)))

model_by_species |>
  mutate(coef = map(model, tidy)) |>
  select(species, coef) |>
  unnest(cols = c(coef))
