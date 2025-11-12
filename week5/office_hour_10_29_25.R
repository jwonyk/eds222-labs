library(tidyverse)
set.seed(123)

# Let's define our population
beta0 <- -10
beta1 <- 2
sigma <- 3

temperature <- rnorm(200, mean = 25, sd = 2)
bleaching <- rnorm(200, 
                   mean = beta0 + beta1 * temperature,
                   sd = sigma)

bleaching_df <- tibble(temperature, bleaching)
ggplot(bleaching_df, aes(temperature, bleaching)) +
  geom_point()

bleaching_lm <- lm(bleaching ~ temperature, bleaching_df)
summary(bleaching_lm)

# To simulate under the null hypothesis
# We have to assume there's no relationship
# WE assume BETA1 is 0

beta0 <- -10
beta1 <- 0
sigma <- 3

bleaching_null <- rnorm(200, 
                        mean = beta0 + beta1 * temperature,
                        sd = sigma)

null_df <- tibble(temperature, bleaching_null)
ggplot(null_df, aes(temperature, bleaching_null)) +
  geom_point()

null_lm <- lm(bleaching_null ~ temperature, null_df)
summary(null_lm)

null_dist <- map_dbl(1:10000, \(i) {
  bleaching_null <- rnorm(200, 
                          mean = beta0 + beta1 * temperature,
                          sd = sigma)
  
  null_df <- tibble(temperature, bleaching_null)
  
  null_lm <- lm(bleaching_null ~ temperature, null_df)
  
  coef(null_lm)[2]

})

hist(null_dist)

# Now let's permuteu
set.seed(123)

# Let's define our poplation
beta0 <- -10
beta1 <- 2
sigma <- 22

temperature <- rnorm(200, mean = 25, sd = 2)
bleaching <- rnorm(200, 
                   mean = beta0 + beta1 * temperature,
                   sd = sigma)

bleaching_df <- tibble(temperature, bleaching)
ggplot(bleaching_df, aes(temperature, bleaching)) +
  geom_point() +
  geom_smooth(method = "lm")

bleaching_lm <- lm(bleaching ~ temperature, bleaching_df)
summary(bleaching_lm)

one_permutation <- mutate(bleaching_df,
                          temperature = sample(temperature))

permutation_est <- coef(lm(bleaching ~ temperature, one_permutation))[2]

permutation_null <- map_dbl(1:10000, \(i) {
  
  one_permutation <- mutate(bleaching_df,
                            temperature = sample(temperature))
  
  coef(lm(bleaching ~ temperature, one_permutation))[2]
  
})

tibble(permutation_null) %>% 
  ggplot(aes(permutation_null)) +
  geom_histogram() +
  geom_vline(xintercept = coef(bleaching_lm)[2], color = "firebrick",
             linewith = 2)

perm_pval <- mean(abs(permutation_null)> coef(bleaching_lm)[2]) 
perm_pval
