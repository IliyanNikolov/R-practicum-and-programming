

#1.1
sample_covariance <- function(x, y, na.rm = FALSE) {
  if(!is.numeric(x)) {
    stop("x is not a numeric vector")
  }
  if(!is.numeric(y)) {
    stop("y is not a numeric vector")
  }
  
  x_len <- length(x)
  if(x_len != length(y)) {
    stop("x and y have different lengths")
  }
  
  x_mean <- mean(x, na.rm = TRUE)
  y_mean <- mean(y, na.rm = TRUE)
  result <- 0
  for(i in 1:x_len) {
    if(na.rm == TRUE && (is.na(x[i]) || is.na(y[i]))) {
      x_len <- x_len - 1
      next
    }
    result <- result + (x[i] - x_mean) * (y[i] - y_mean)
  }
  if(x_len < 2) {
    stop("not enough available observations")
  }
  result / (x_len - 1)
}

# sample_covariance(c(2,4,6,8,10), c(3,7,10,14,17)) == 17.5
# sample_covariance(c(3,6,9,12,15), c(20,17,13,9,4)) == -30


#1.2
sample_standard_deviation <- function(arr, na.rm = FALSE) {
  result <- 0
  arr_mean <- mean(arr, na.rm=na.rm)
  arr_len <- length(arr)
  for(n in arr){
    if(na.rm == TRUE && is.na(n)) {
      arr_len <- arr_len - 1
      next
    }
    result <- result + (n - arr_mean)^2
  }
  result / (arr_len - 1) # arr_len can't be less than 2 because sample_covariance would have already stopped
}

sample_corelation <- function(x, y, na.rm = FALSE) {
  covariance <- sample_covariance(x, y, na.rm)
  x_std_dev <- sample_standard_deviation(x, na.rm)
  y_std_dev <- sample_standard_deviation(y, na.rm)
  
  covariance/sqrt(x_std_dev * y_std_dev)
}


#1.3
library(dplyr)
library(ggplot2)

summary(diamonds)


#1.4
filter(diamonds, price >= 600)


#1.5
diamonds <- filter(diamonds, price >= 600)
average_price_per_cut <- summarise(group_by(diamonds, cut), appc = mean(price, na.rm = TRUE))
arrange(average_price_per_cut, appc)
# The Ideal cut level has lowest average price and the Premium cut level has the highest


#1.6
diamonds <- mutate(diamonds, log_carat = log(carat), log_price = log(price))
sample_corelation(diamonds$log_carat, diamonds$log_price, na.rm = TRUE)
# The sample covariance is close to 1 but this isn't enough to say that there is a linear relationship between log_carat and log_price


#1.7
data_to_plot <- filter(diamonds, clarity == "I1" | clarity == "IF")

ggplot(data = data_to_plot, mapping = aes(x = log_carat, y = log_price, color = clarity)) +
  geom_point() +
  geom_smooth(se = FALSE)

