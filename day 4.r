path = "input day 4.txt"
path = "input day 4 test.txt"

library(tidyverse)

data <- readLines(path, warn = F)
winningnumbers <- as.integer(unlist(strsplit(data[1], ",")))
numbers <- expand_grid(people = 1:(length(unlist(strsplit(paste0(data[-1:-2], collapse = " "), "\\s+")))/25), rows = 1:5, cols = 1:5)
numbers$number <- as.integer(unlist(strsplit(paste0(data[-1:-2], collapse = " "), "\\s+")))

week4 <- function(bingonumbers, winningnumbers, data, best) {
  rolling <- function(bingonumbers, winningnumbers) {
    bingo <- c()
    for(i in 1:length(bingonumbers)) {
      bingo[i] <- which(bingonumbers[i] == winningnumbers)
    }
    bingo
  }
  data$bingo <- rolling(data$number, winningnumbers)
  
  person <- data %>% 
    group_by(people, rows) %>% mutate(bingorows = max(bingo)) %>% ungroup() %>% 
    group_by(people, cols) %>% mutate(bingocols = max(bingo)) %>% ungroup()
  
  if(best == T) {
    winnerscore <- person %>% pivot_longer(cols = c(bingorows, bingocols), names_to = "rowcol", values_to = "score") %>% 
      group_by(people, score) %>% summarise(freq = n()) %>% 
      summarise(score = min(score)) %>% filter(score == min(score))
  } else {
    winnerscore <- person %>% pivot_longer(cols = c(bingorows, bingocols), names_to = "rowcol", values_to = "score") %>% 
      group_by(people, score) %>% summarise(freq = n()) %>% 
      summarise(score = min(score)) %>% filter(score == max(score))
  }
  winningnumbers[winnerscore$score] * data %>% filter(people == winnerscore$people) %>% filter(bingo > winnerscore$score) %>% pull(number) %>% sum()
}
week4(numbers$number, winningnumbers, numbers, F) # T for best/F for worst




