
# input -------------------------------------------------------------------

day3 <- readLines("input day 3.txt")


# Part 1 ------------------------------------------------------------------

powerconsumption <- function(data) {
  cols = length(strsplit(data[1], "")[[1]])
  datamatrix <- matrix(as.numeric(unlist(strsplit(data, ""))), ncol = cols, byrow = T)
  gamma = round(colSums(datamatrix)/nrow(datamatrix))
  epsilon = 1 - gamma
  strtoi(paste0(gamma, collapse = ""), base = 2) * strtoi(paste0(epsilon, collapse = ""), base = 2)
}
powerconsumption(day3)


# Part 2 ------------------------------------------------------------------

generator <- function(data) {
  cols = length(strsplit(data[1], "")[[1]])
  datamatrix <- matrix(as.numeric(unlist(strsplit(data, ""))), ncol = cols, byrow = T)
  for(i in 1:ncol(datamatrix)) {
    datamatrix <- datamatrix[which(datamatrix[,i] == round(sum(datamatrix[,i])/nrow(datamatrix)+0.0001)),]
  }
  datamatrix
}

scrubber <- function(data) {
  cols = length(strsplit(data[1], "")[[1]])
  datamatrix <- matrix(as.numeric(unlist(strsplit(data, ""))), ncol = cols, byrow = T)
  #datavector <- c()
  for(i in 1:ncol(datamatrix)) {
    datamatrix <- datamatrix[which(datamatrix[,i] == (1-round(sum(datamatrix[,i])/nrow(datamatrix)+0.0001))),]
#    datavector[i] <- (1-round(sum(datamatrix[,i])/nrow(datamatrix)+0.0001))
    if(is.matrix(datamatrix) == F) {
      break
    }
  }
  datamatrix
}

lifesupport <- function(data) {
  oxygen <- generator(data)
  co2 <- scrubber(data)
  strtoi(paste0(oxygen, collapse = ""), base = 2) * strtoi(paste0(co2, collapse = ""), base = 2)
}
lifesupport(day3)
