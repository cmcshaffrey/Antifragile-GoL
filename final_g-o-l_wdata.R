#This is the most coherent iteration of the project thus far.


#Possibility that the cell will turn on or off (can't be negative)
mutation.probability <- 0
#0.0001

#Size of the board
board_size <- 1000

#The duration that the simulation is run for
duration <- 1000

#The duration that the test runs for without the volatility measures.
#This is here because I need to see how many cells are on due to life
#contingencies in the game and not because of the randomness I am injecting.
test_duration <- 100

#In cycles, the amount of time the environment will go without volatility
#(has to be set to at least 1 due to if-statement in life_step() )
volatility_period <- 1
#30

#mutation function
mutations <- matrix(
  sample(c(0,1), board_size, prob = c(1-mutation.probability, mutation.probability), replace=T), 
  nrow=50)

life_step <- function(d) {
  # form the neighboring sums
  rows <- dim(d)[[1]]
  cols <- dim(d)[[2]]
  #Edit lines 72-79 for wrap-around 
  d_eu <- d[c(2:rows, 1), ]
  d_ed <- d[c(rows, 1:(rows-1)), ]
  d_le <- d[ ,c(2:cols, 1)]
  d_re <- d[ ,c(cols, 1:(cols-1))]
  d_lu <- d[c(2:rows, 1), c(2:cols, 1)]
  d_ru <- d[c(2:rows, 1), c(cols, 1:(cols-1))]
  d_ld <- d[c(rows, 1:(rows-1)), c(2:cols, 1)]
  d_rd <- d[c(rows, 1:(rows-1)), c(cols, 1:(cols-1))]
  pop <- d_eu + d_ed + d_le + d_re + d_lu + d_ru + d_ld + d_rd
  d <- (pop==3) | (d & (pop>=2) & (pop<=3))
  d
}

#Run simulation function
run_simulation <- function(mutation.probability, volatility_period){
  #BOARD SIZE
  board<-matrix(
    sample(c(TRUE, FALSE), board_size, replace = T),
    nrow = 50
  )
  
  #The life trackers for during and between trials
  life_tracker <- matrix(
    sample(c(0), board_size, replace = T),
    nrow = 50
  )
  
  #This one does the same thing, but only for the test_duration
  life_tracker_test <- matrix(
    sample(c(0), board_size, replace = T),
    nrow = 50
  )
  
  #The number of cells that are on during each step.
  #This would be for analyzing data within a simulation, but I cannot
  #do that yet so it is just an artifact for this iteration of the code. 
  cell_num_tracker <- 0
  
  #How many transitions take place in the board with volatility
  for(i in 1:duration){
    if (i %% volatility_period == 0){
      mutations <- matrix(
        sample(c(0,1), board_size, prob = c(1-mutation.probability, mutation.probability), replace=T), 
        nrow=50)
      board <- abs(board - mutations)
    }
    board<-life_step(board)
    #This relates to the artifact comment above
    cell_num_tracker[i] <- sum(board)
  }
  
  #The cells that are on at the end of the volatility phase
  life_tracker <- life_tracker + board
  
  #How many transitions will take place on the board without volatility
  for (j in 1:test_duration) {
    board<-life_step(board)
    #This relates to the artifact comment above
    cell_num_tracker[i] <- sum(board)
  }
  
  #The cells that are on at the end of the test_duration
  life_tracker_test <- life_tracker_test + board
  
  #ANALYSIS:
  #I need to see how often the rate is the same for each of the cells based off
  #of the life tracker. So...
  life_averages_volatility <- (life_tracker / duration)
  
  life_averages_test <- (life_tracker_test / test_duration)
  
  cell_stability_volatility <- mean(life_averages_volatility)
  
  cell_stability_test <- mean(life_averages_test)
  
  #Since R can only return one output in a function I need to make it a list
  #so I can have both pieces of information merged as one object.
  cell_stability_info <- list("volatility_stability" = cell_stability_volatility,
                              "test_phase_stability" = cell_stability_test)
  
  return(cell_stability_info)
}


#Running multiple simulations
run_simulations <- function(mutation.probability, volatility_period, n = 10){
  counter_1 = 0
  counter_2 = 0
  for (i in 1:n) {
    result <- run_simulation(mutation.probability, volatility_period)
    counter_1 <- counter_1 + result$volatility_stability
    counter_2 <- counter_2 + result$test_phase_stability
  }
  
  counter_1<-(counter_1/n)
  counter_2<-(counter_2/n)
  
  volatility_and_test_stability <- list("volatility_stability" = counter_1,
                                        "test_phase_stability" = counter_2)

  return(volatility_and_test_stability)
}

mutation_rate_pool <- seq(0, 0.5, by = 0.002)
volatility_period_pool <-seq(1, 1000, by = 5)

grid_possib <- expand.grid(m = mutation_rate_pool, v = volatility_period_pool)
grid_possib$volatility_stability <- NA
grid_possib$test_phase_stability <- NA

for (r in 1:nrow(grid_possib)) {
  result <-  run_simulations(grid_possib$m[r], grid_possib$v[r])
  grid_possib$volatility_stability[r] <- result$volatility_stability
  grid_possib$test_phase_stability[r] <- result$test_phase_stability
  print(r)
}

#I am backing up the grid_possib data in a different variable just so it is 
#saved in a couple locations if I need to back track.
saved_data <- grid_possib

#I am going to load in some libraries that I might need to do the analysis
library(dplyr)
library(ggplot2)
library(tidyr)

#I am going to add another column to the data frame here so that I can calculate
#the difference between the volatility and no volatility phases for each
#parameter set of simulations. 
comparison_data <- saved_data %>%
  mutate(absolute_diff = abs(volatility_stability - test_phase_stability))

#Now that I have that extra comparison problem, I will try to make some graphs
#to visualize what is happening as the parameter combinations change.

#This is for the volatility phase ecosystem cell stability
ggplot(comparison_data, aes(x = m, y = volatility_stability, colour = v)) +
  geom_point() 

#This is for the test phase ecosystem cell stability
ggplot(comparison_data, aes(x = m, y = test_phase_stability, colour = v)) +
  geom_point() 

#This is for the absolute difference ecosystem cell stability
ggplot(comparison_data, aes(x = m, y = absolute_diff, colour = v)) +
  geom_point() 

#This is to get the more refined graphs where the v-value is 200 and 20. 

comparison_refine1 <- comparison_data_2 %>%
  filter(v < 20)

ggplot(comparison_refine1, aes(x = m, y = volatility_stability, colour = v)) +
  geom_point()

comparison_refine2 <- comparison_data_2 %>%
  filter(v < 20)

ggplot(comparison_refine2, aes(x = m, y = volatility_stability, colour = v)) +
  geom_point() 
