library(tidyverse)
library(modelr)
#wraps around base R's functions to make them work naturally in a pipe

options(na.action = na.warn)
#In order to get a warning that missing values are dropped while estimating the model


ggplot(sim1, aes(x, y)) + 
  geom_point()

tibble(sim1)

#capture the pattern and make it explicit
#provide basic form of the model (linear relationship)

#generate random slopes and intercepts

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)


ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 

#turn model family into R function

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

#calculate a single number as measure of the 'distance'

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)
#> [1] 2.665212

#Now for all the intercept slope vectors given by the simulated 'models'
#helper function sim1_dist is defined below that take the 2 dimensional parameter vector as argument for sim1 data only

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

#remember: map2(x,y,f) is basically f(x,y) element wise

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

#lower dist means better model (closer to data)

#10 best models

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

#Treating the 250 models as observations

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

#Zeroing in on the parameters a bit more systematically

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 

# Newton-Raphson using optim()
# first argument is the starting point
# par are the best set of parameters
# data=sim1 is the additional argument passed to the function measure_distance
# https://www.magesblog.com/post/2013-03-12-how-to-use-optim-in-r/


best <- optim(c(0, 0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])

#Alternatively one can use lm() for fitting general linear models

sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

#Visualizing models (understanding patterns)

# Visualizing predictions
#Generate an evenly-spaced grid of values

grid <- sim1 %>% 
  data_grid(x) 
grid

#Add predictions
grid <- grid %>% 
  add_predictions(sim1_mod) 
grid

#Plot predictions
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)


#Visualizing residuals
#Add residuals to original dataset (actual y values needed)

sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1

#Check spread of residuals via a frequency polygon

ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)

#Plotting the residuals on the data (independent variable)
ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() 

#Formulas in R tutorial available @ https://www.datacamp.com/community/tutorials/r-formula-tutorial

df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)


model_matrix(df, y ~ x1)

model_matrix(df, y ~ x1 - 1)

model_matrix(df, y ~ x1 + x2)

#Note that in the the above translations (formula to matrix) values of y do not matter

#Formula notation for categorical variables

df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
)
model_matrix(df, response ~ sex)

#Formula notation for interactions

# Interactions: Continuous together with categorical predictors

ggplot(sim3, aes(x1, y)) + 
  geom_point(aes(colour = x2))

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)


grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2)
grid


ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)

sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)

# Interactions: Continuous and Continuous

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)
grid

#Formula notation for transformations
# Use I() if transformation involves ^,*,-,+


df <- tribble(
  ~y, ~x,
  1,  1,
  2,  2, 
  3,  3
)
model_matrix(df, y ~ x^2 + x)
model_matrix(df, y ~ I(x^2) + x)

#Didn't quite get the model matrix after transformation by poly(x,2)
# Transformation involves a polynomial model (approximation using Taylor's theorem)

model_matrix(df, y ~ poly(x, 2))

library(splines)
model_matrix(df, y ~ ns(x, 2))
#Using natural splines since polynomials go outside the range

#Approximating a non-linear function
sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)

ggplot(sim5, aes(x, y)) +
  geom_point()

mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)
