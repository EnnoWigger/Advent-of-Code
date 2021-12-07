data <- readLines("input day 5.txt", warn = F)
data

numbers <- data %>% str_match_all("[:digit:]+") %>% unlist() %>% as.integer() %>% matrix(ncol = 4, byrow = T) %>% as_tibble()
names(numbers) <- c("x1", "y1", "x2", "y2")
numbers

part1 <- function(numbers) {
  plus <- tibble(rows = 0, cols = 0)
  for(i in 1:nrow(numbers)) {
    if(numbers$x1[i]-numbers$x2[i] == 0 | numbers$y1[i]-numbers$y2[i] == 0) {
      plus <- rbind(plus, tibble(rows = numbers$x1[i]:numbers$x2[i], cols = numbers$y1[i]:numbers$y2[i]))
    }
  }
  plus  %>% filter(rows > 0) %>% mutate(position = paste0(rows,",",cols)) %>% group_by(position) %>% summarise(freq = n()) %>% count(freq) %>% filter(freq > 1) %>% pull(n) %>% sum()
}
part1(numbers)

part2 <- function(numbers) {
  positions <- tibble(rows = 0, cols = 0)
  for(i in 1:nrow(numbers)) {
    positions <- rbind(positions, tibble(rows = numbers$x1[i]:numbers$x2[i], cols = numbers$y1[i]:numbers$y2[i]))
  }
  positions %>% filter(rows > 0) %>% mutate(position = paste0(rows,",",cols)) %>% group_by(position) %>% summarise(freq = n()) %>% count(freq) %>% filter(freq > 1) %>% pull(n) %>% sum()
}
part2(numbers)




