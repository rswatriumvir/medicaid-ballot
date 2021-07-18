# load libraries
library(tidyverse)
library(rsample)

# data
df = mtcars %>% as_tibble()

# split data into testing and train set
split = initial_split(df, prop = 3/4)
train = training(split)
test = testing(split)


# a couple of models that predict mpg
mod1 = lm(mpg ~ wt, data = train)
mod2 = lm(mpg ~ hp + wt, data = train)
mod3 = lm(mpg ~ hp + wt + vs, data = train)

# make predictions on the training set
pred1 = augment(mod1)
pred2 = augment(mod2)
pred3 = augment(mod3)

# which model did best on the training data? 
pred1 %>% 
  summarise(sum_squared_errors = sum((mpg - .fitted)^2))
pred2 %>% 
  summarise(sum_squared_errors = sum((mpg - .fitted)^2))
pred3 %>% 
  summarise(sum_squared_errors = sum((mpg - .fitted)^2))

# make predictions on the test set
pred1 = augment(mod1, newdata = test)
pred2 = augment(mod2, newdata = test)
pred3 = augment(mod3, newdata = test)


# which model did best on the test data? 
pred1 %>% 
  summarise(sum_squared_errors = sum((mpg - .fitted)^2))
pred2 %>% 
  summarise(sum_squared_errors = sum((mpg - .fitted)^2))
pred3 %>% 
  summarise(sum_squared_errors = sum((mpg - .fitted)^2))


