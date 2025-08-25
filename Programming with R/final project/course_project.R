# Problem 1

sim_clt <- function(i, n, p) {
  if(!is.integer(i) || i < 1000 || i > 10000) {
    stop("i should be an integer between 1000 and 10000")
  }
  if(!is.integer(n) || n < 90 || n > 900 || n %% 2 == 1) {
    stop("n should be an even integer between 90 and 900")
  }
  if(!is.double(p) || p < 0 || p > 1) {
    stop("p should be a real number in the range [0, 1]")
  }
  
  mean = (1 - p) / p
  std_dev = sqrt((1 - p) / p^2)
  result <- c()
  for(j in 1:i) {
    x1 <- rnorm(n / 2, mean, std_dev)
    x2 <- rgeom(n / 2, p)
    x <- sample(append(x1, x2), n, replace = FALSE)
    result <- append(result, (mean(x) - mean) * sqrt(n) / std_dev)
  }
  result
}

r <- sim_clt(10000L, 900L, 0.5)
sprintf("mean of the values in the returned vector: %f",mean(r))
sprintf("and standard deviation: %f", sd(r))
qqnorm(r)
qqline(r)
# it looks like the values follow a normal distribution




# Problem 2

library(tidymodels)
library(ggplot2)

# the dependent variable is mpg and the independent variable is wt
# because we want to estimate the relationship mpg = f(wt)


#2.2
set.seed(42)
data_split <- initial_split(mtcars, prop = 30 / 32)
training_data <- training(data_split)
test_data <- testing(data_split)


#2.3
lm_mod <- linear_reg()
lm_fit <- lm_mod %>%
  fit(mpg ~ wt, data = training_data)
# the fitted function will be a member of the family of linear functions


#2.4
tidy(lm_fit)
# for this data split the estimated function f_hat(wt) = -4.99*wt + 36


#2.5
ggplot(training_data, aes(wt, mpg)) +
  geom_point() +
  geom_abline(intercept = 36, slope = -4.99, color = "red")


#2.6
augment(lm_fit, new_data = test_data) %>%
  rmse(mpg, .pred)

