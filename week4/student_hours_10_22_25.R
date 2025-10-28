library(tidyverse)
library(palmerpenguins)

glimpse(penguins)

# body_mass ~ bill_length + island
penguin_mod <- lm(body_mass_g ~ bill_length_mm + island, data = penguins)
summary(penguin_mod)

penguin_pred <- expand_grid(
  bill_length_mm = seq(32, 60, length.out = 2),
  island = unique(penguins$island)
) %>%
  mutate(body_mass_g = predict(penguin_mod, newdata = .))