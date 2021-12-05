day2 <- read.csv("input day 2.txt", header = F)

positiondepth <- function(data) {
  datadf <- as.data.frame(matrix(unlist(strsplit(data[,1], " ")), ncol = 2, byrow = T))
  datadf[,2] <- as.numeric(datadf[,2])
  part1 <- (sum(datadf[,2][which(datadf[,1] == "down")]) - sum(datadf[,2][which(day2df[,1] == "up")])) * sum(day2df[,2][which(day2df[,1] == "forward")])
  part1
  
  datadf[,3] <- ifelse(datadf[,1] == "forward", 0, ifelse(datadf[,1] == "up", datadf[,2] * -1, datadf[,2]))
  datadf[,4] <- cumsum(datadf[,3])
  position <- c(ifelse(datadf[,1] == "forward", datadf[,2], 0))
  depth <- c(ifelse(datadf[,1] == "forward", datadf[,2] * datadf[,4], 0))
  part2 <- sum(position) * sum(depth)
  
  data.frame("Part1" = part1, "Part2" = part2)
}
positiondepth(day2)
