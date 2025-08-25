number_of_simulations <- 100000

#1

sim.balls <- function() {
  ball_1 <- sample(c(1:8), 1)
  ball_2 <- sample(c(1:8), 1)
  ball_2 == ball_1
}

sum(replicate(number_of_simulations, sim.balls())) / number_of_simulations


#2

sim.socks <- function() {
  socks <- sample(c(0, 0, 1, 1, 2, 2), 2,  replace = F)
  socks[1] == socks[2]
}

sum(replicate(number_of_simulations, sim.socks())) / number_of_simulations


#3

sim.keys <- function() {
  keys <- sample(c(rep(0, 3), 1), 4, replace = F)
  keys[4] == 1
}

sum(replicate(number_of_simulations, sim.keys())) / number_of_simulations


#4

sim.exam <- function() {
  questions <- sample(c(rep(1, 17), rep(0, 3)), 2, replace = F)
  sum(questions) == 1
}

sum(replicate(number_of_simulations, sim.exam())) / number_of_simulations


#5

sim.birthday <- function() {
  birthdays <- sample(c(1:365), 25, replace = T)
  length(birthdays) > length(unique(birthdays))
}

sum(replicate(number_of_simulations, sim.birthday())) / number_of_simulations


#6

sim.presents <- function() {
  presents <- sample(c(1:20), 20, replace = F)
  any(presents == c(1:20))
}

sum(replicate(number_of_simulations, sim.presents())) / number_of_simulations


#7

sim.ants <- function() {
  moves <- sample(c(0, 1), 3, replace = T)
  sum(moves) == 3 || sum(moves) == 0
}

sum(replicate(number_of_simulations, sim.ants())) / number_of_simulations


#8

sim.eggs_A <- function() {
  eggs <- sample(c(rep(0, 6), rep(1, 2)), 8, replace = F)
  player_1 <- eggs[1] + eggs[3] + eggs[5] + eggs[7]
  player_2 <- eggs[2] + eggs[4] + eggs[6] + eggs[8]
  player_1 == 2 || player_2 == 2
}

sum(replicate(number_of_simulations, sim.eggs_A())) / number_of_simulations


sim.eggs_B <- function() {
  eggs <- sample(c(rep(0, 6), rep(1, 2)), 8, replace = F)
  player_1 <- eggs[1] + eggs[3] + eggs[5] + eggs[7]
  player_2 <- eggs[2] + eggs[4] + eggs[6] + eggs[8]
  player_1 == 1
}

sum(replicate(number_of_simulations, sim.eggs_B())) / number_of_simulations


sim.eggs_C <- function() {
  eggs <- sample(c(rep(0, 6), rep(1, 2)), 8, replace = F)
  player_1 <- eggs[1] + eggs[3] + eggs[5] + eggs[7]
  player_2 <- eggs[2] + eggs[4] + eggs[6] + eggs[8]
  player_1 == 2
}

sum(replicate(number_of_simulations, sim.eggs_C())) / number_of_simulations


sim.eggs_D <- function() {
  eggs <- sample(c(rep(0, 6), rep(1, 2)), 8, replace = F)
  player_1 <- eggs[1] + eggs[3] + eggs[5] + eggs[7]
  player_2 <- eggs[2] + eggs[4] + eggs[6] + eggs[8]
  player_2 == 2
}

sum(replicate(number_of_simulations, sim.eggs_D())) / number_of_simulations


#9

sim.test <- function() {
  answers <- sample(c(0, 0, 0, 1), 10, replace = T)
  sum(answers) > 4
}

sum(replicate(number_of_simulations, sim.test())) / number_of_simulations


#10

sim.sits_A <- function() {
  on_time <- sample(c(0, 1), 143, replace = T, prob = (c(0.08, 0.92)))
  sum(on_time) < 139
}

sum(replicate(number_of_simulations, sim.sits_A())) / number_of_simulations

sim.sits_B <- function() {
  on_time <- sample(c(0, 1), 143, replace = T, prob = (c(0.08, 0.92)))
  sum(on_time) == 137
}

sum(replicate(number_of_simulations, sim.sits_B())) / number_of_simulations


#11

sim.green_ball_A <- function() {
  dice <- sample(c(1:6), 1, replace = F)
  if(dice == 6){
    box <- c(0, 0, 1, 1)
  }
  else{
    box <- c(0, 0, 0, 0, 1)
  }
  sample(box, 1)
}

sum(replicate(number_of_simulations, sim.green_ball_A())) / number_of_simulations

sim.green_ball_B <- function() {
  #dice <- sample(c(1:6), 1, replace = F)
  #if(dice == 6){
  #  box <- c(0, 0, 1, 1)
  #}
  #else{
  #  box <- c(0, 0, 0, 0, 1)
  #}
  #sample(box, 1)
  
  #ebal sum mu maikata
  
}

#sum(replicate(number_of_simulations, sim.green_ball_B())) / number_of_simulations


#12

sim.coin_A <- function() {
  coin <- sample(c(0, 0, 1, 2, 2), 1)
  if(coin == 0) {
    res <- 1
  }
  if(coin == 1) {
    res <- 0
  }
  if(coin == 2) {
    res <- sample(c(0, 1), 1)
  }
  res
}

sum(replicate(number_of_simulations, sim.coin_A())) / number_of_simulations

sim.coin_B <- function() {
  coin <- sample(c(0, 0, 1, 2, 2), 1)
  if(coin == 0) {
    res <- c(1, 0)
  }
  if(coin == 1) {
    res <- c(0, 1)
  }
  if(coin == 2) {
    res <- c(sample(c(0, 1), 1), 2)
  }
  res
}

gen <- replicate(number_of_simulations, sim.coin_B())

sum(gen[1,] == 1 & gen[2,] == 2) / sum(gen[1,] == 1)


#13

sim.cards <- function() {
  card <- sample(c(0, 1, 2), 1)
  if(card == 0) {
    res <- c(1, 0)
  }
  if(card == 1) {
    res <- c(sample(c(0, 1), 1), 1)
  }
  if(card == 2) {
    res <- c(0, 2)
  }
  res
}

gen <- replicate(number_of_simulations, sim.cards())
sum(gen[1,] == 1 & gen[2,] == 0) / sum(gen[1,] == 1)


#14

sim.draw_balls <- function() {
  balls <- sample(c(1:99), 4, replace = F)
  all(balls[1] >= balls)
}

sum(replicate(number_of_simulations, sim.draw_balls())) / number_of_simulations


#15

sim.order <- function() {
  order <- sample(c(rep(0, 18), 1, 1), 20, replace = F)
  indeces <- which(order == 1)
  indeces[1] + 1 == indeces[2]
}

sum(replicate(number_of_simulations, sim.order())) / number_of_simulations


#16

sim.cards_ace <- function() {
  hands <- sample(c(rep(0, 48), rep(1, 4)), 52, replace = F)
  sum(hands[1:13]) == sum(hands[14:26]) & sum(hands[14:26]) == sum(hands[27:39] & sum(hands[40:52]))
}

sum(replicate(number_of_simulations, sim.cards_ace())) / number_of_simulations


#27

1 - phyper(1, 90, 2910, 50)


#28

#a
1 - pgeom(3, 0.1)

#b
1 - pnbinom(39, 10, 0.1)


#29

1 - pbinom(2, 360, 1/90)


#30

sim.draws <- function() {
  cards <- sample(c(rep(0, 39), rep(1, 13)), 10, replace = F)
  sum(cards) > 2
}

sum(replicate(number_of_simulations, sim.draws())) / number_of_simulations


#31

1 - pbinom(1, 52, 0.25)
