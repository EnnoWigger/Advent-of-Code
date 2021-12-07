horizontal <- c(16,1,2,0,4,2,7,1,2,14)
horizontal <- readLines("input day 7.txt", warn = F) %>% str_split(",") %>% unlist() %>% as.integer()

medianhorizontal <- horizontal %>% median
sum(sqrt((horizontal - medianhorizontal)^2)) # 473601

meanhorizontal <- horizontal %>% mean() %>% round() -1
sqrts <- sqrt((horizontal - meanhorizontal)^2)
sum(sqrts*(sqrts+1)/2) #96744904
