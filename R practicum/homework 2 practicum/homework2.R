#1


n_1 <- 10
n_2 <- 20
n_3 <- 30
n_4 <- 40
n_5 <- 50
n_6 <- 60

numbers_generated <- c(n_1, n_2, n_3, n_4, n_5, n_6)

prob.normal <- function(n) {
  w1 <- rexp(n, 1)
  w2 <- rexp(n, 1)
  x <- w1 + w2
  
  shapiro.test(x)$p.value
}

number_of_simulations <- 10000


is_n_1_normal <- sum(replicate(number_of_simulations, prob.normal(n_1)) > 0.05) / number_of_simulations
is_n_2_normal <- sum(replicate(number_of_simulations, prob.normal(n_2)) > 0.05) / number_of_simulations
is_n_3_normal <- sum(replicate(number_of_simulations, prob.normal(n_3)) > 0.05) / number_of_simulations
is_n_4_normal <- sum(replicate(number_of_simulations, prob.normal(n_4)) > 0.05) / number_of_simulations
is_n_5_normal <- sum(replicate(number_of_simulations, prob.normal(n_5)) > 0.05) / number_of_simulations
is_n_6_normal <- sum(replicate(number_of_simulations, prob.normal(n_6)) > 0.05) / number_of_simulations

is_normal <- c(is_n_1_normal, is_n_2_normal, is_n_3_normal, is_n_4_normal, is_n_5_normal, is_n_6_normal)

plot(numbers_generated, is_normal, type = "l", col = "blue")



#2


n_7 <- 50
n_8 <- 100
n_9 <- 200
n_10 <- 500

number_of_draws <- c(n_7, n_8, n_9, n_10)

number_of_simulations <- 10000

sim.draws <- function(n) {
  # 0 indicates white and yellow balls and 1 is for green balls
  box <-c(rep(0, 8), rep(1, 2))
  
  draw <- sample(box, n, replace = T)
  prop.test(sum(draw), n, p = 1 / 5)$p.value
}

probability_n_7 <- sum(replicate(number_of_simulations, sim.draws(n_7)) > 0.05) / number_of_simulations
probability_n_8 <- sum(replicate(number_of_simulations, sim.draws(n_8)) > 0.05) / number_of_simulations
probability_n_9 <- sum(replicate(number_of_simulations, sim.draws(n_9)) > 0.05) / number_of_simulations
probability_n_10 <- sum(replicate(number_of_simulations, sim.draws(n_10)) > 0.05) / number_of_simulations

probabilities <- c(probability_n_7, probability_n_8, probability_n_9, probability_n_10)

plot(number_of_draws, probabilities, type = "l", col = "blue")