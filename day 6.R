
# day 6 -------------------------------------------------------------------

library(tidyverse)

data <- readLines("input day 6.txt", warn = F)
fish <- data %>% str_split(",") %>% unlist() %>% as.numeric() 
fish <- c(3,4,3,1,2)
fish

school <- function(fish, days) {
  lanternfish = fish
  day = c(1:days)
  for(i in 1:days) {
    if(all(lanternfish > 0)) {
      lanternfish = lanternfish - 1  
    } else {
      lanternfish = lanternfish - 1
      preggo <- which(lanternfish == -1)
      lanternfish[preggo] <- 6
      lanternfish = c(lanternfish, rep(8, times = length(preggo)))
    }
    day[i] <- length(lanternfish)
  }
  day[length(day)]
}
school(fish, 80)

fishmatrix <- matrix(as.integer(0), nrow = length(fish), ncol = 9)
for(i in 1:length(fish)) {
  fishmatrix[i, (fish[i]+1)] <- 1L
}
fishmatrix

matrixfish <- function(data, days) {
  matrix1 <- matrix(as.integer(0), nrow = length(data), ncol = 9)
  for(i in 1:length(data)) {
    matrix1[i, (data[i]+1)] <- 1L
  }
  for(i in 1:days) {
    matrix2 <- matrix1
    matrix1[,9] <- matrix2[,1]
    matrix1[,8] <- matrix2[,9]
    matrix1[,7] <- matrix2[,1] + matrix2[,8]
    matrix1[,6] <- matrix2[,7]
    matrix1[,6] <- matrix2[,7]
    matrix1[,5] <- matrix2[,6]
    matrix1[,4] <- matrix2[,5]
    matrix1[,3] <- matrix2[,4]
    matrix1[,2] <- matrix2[,3]
    matrix1[,1] <- matrix2[,2]
  }
  sum(matrix1)
}
matrixfish(fish, 256) %>% as.character()



