#1

share_apples <- function() {
  x <- c(rep(0, 20), rep(1, 4)) 
  permutation_x <- sample(x, length(x), replace = F)
  box1 <- permutation_x[1:6]
  box2 <- permutation_x[7:12]
  box3 <- permutation_x[13:18]
  box4 <- permutation_x[19:24]
  sum(box1) == 1 && sum(box2) == 1 && sum(box3) == 1 && sum(box4) == 1
}

prob.apples <- function(Nrep) {
  res <- replicate(Nrep, share_apples())
  sum(res) / length(res)
}

prob.apples(100000)


#2

gen_n_numbers <- function(n, lambda) {
  x_array <- rexp(n, lambda)
  mean(x_array)
}

gen_means <- function(N, n, lambda) {
  x_means <- replicate(N, gen_n_numbers(n, lambda))
  x_means
}

gen_F_hat <- function(t, x_means, N) {
  sum(x_means <= t) / N
}

solution <- function(n) {
  lambda <- 1/5
  N <- 10000
  x_means = gen_means(N, n, lambda)
  
  x_axis = seq(-10, 10, by = 0.1)
  y_F_hat_axis = sapply(x_axis, gen_F_hat, x_means = x_means, N = N)
  
  y_norm_axis = dnorm(x_axis, 5, 5 / sqrt(n))
  
  plot(x_axis, y_F_hat_axis, type = "l", col = "blue", ylim = range(c(y_F_hat_axis, y_norm_axis)))
  lines(x_axis, y_norm_axis, col = "red")
}

solution(30)
solution(120)
solution(200)
