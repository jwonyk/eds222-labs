library(tidyverse)

# `dnorm()`
#1. Define the possible outcomes

foo <- tibble(x = seq(0, 100, length.out = 100))

#2. Choose our parameters
mu <- 45
sigma <- 16

#3. Calculate Density
foo <- mutate(foo, density = dnorm(x, mean = mu, sd = sigma))

#4. Plot it!
ggplot(foo, aes(x, density)) +
  geom_line()



# `dpois()`
#1. Define the possible outcomes

foo <- tibble(x = seq(0, 20, by = 1))

#2. Choose our parameters
lambda <- 5

#3. Calculate Density
foo <- mutate(foo, mass = dpois(x, lambda = lambda))

#4. Plot it!
ggplot(foo, aes(x, mass)) +
  geom_line()



# `dgamma()`
#1. Define the possible outcomes

foo <- tibble(x = seq(0, 10, by = 0.1))

#2. Choose our parameters
shape <- 4
rate <- 1

#3. Calculate Density
foo <- mutate(foo, density = dgamma(x, shape = shape, rate = rate))

#4. Plot it!
ggplot(foo, aes(x, density)) +
  geom_line()



# `dbeta()`
#1. Define the possible outcomes
# Beta(2, 7)
beta_pdf <- tibble(x = seq(0, 1, length.out = 100))

#2. Choose our parameters
a <- 2
b <- 7

#3. Calculate Density
beta_pdf <- mutate(beta_pdf,
                   pdf = dbeta(x, shape1 = a, shape2 = b))

#4. Plot it!
ggplot(beta_pdf, aes(x, pdf)) +
  geom_line(linewidth = 2) +
  theme_classic()



#### My example of `dgeom()`
#1. Define the possible outcomes

foofa <- tibble(
  x = seq(0, 100, length.out = 100)
)

#2. Choose our parameters
mu <- 10
sigma <- 5

#3. Calculate Density
foofa <- mutate(foofa, prob = dgeom(x, p = 0.3))

#4. Plot it!
ggplot(foofa, aes(x, prob)) +
  geom_histogram()



