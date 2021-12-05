
# input -------------------------------------------------------------------

day1 <- as.numeric(readLines("input day 1.txt"))


# part 1 ------------------------------------------------------------------

measurements <- function(data) {
  x = 0
  for(i in 1:(length(data)-1)) {
    if(data[i] < data[i+1]) {
      x = x + 1
    } else {
      x = x
    }
    x
  }
  x
}
measurements(day1)


# part 2 ------------------------------------------------------------------

threemeasurements <- function(data) {
  x = 0
  for(i in 1:(length(data)-3)) {
    if(sum(data[i]+data[i+1]+data[i+2]) < sum(data[i+1]+data[i+2]+data[i+3])) {
      x = x + 1
    } else {
      x = x
    }
    x
  }
  x
}
threemeasurements(day1)
