# converting degree to radian
degreesToRadians <- function(x) {
  radian <- x * (pi/180)
  return(radian)
}

degreesToRadians(360)


library(stringr)
x <- rep(NA, 5)
my_strings <- c("a", "a ", "a c", "a ca", "a cat")
for (i in 1:5) {
  print(my_strings[i])
  x[i] <- str_length(my_strings[i])
  print(x)
}

for (i in 1:100) {
  if (i %% 3 == 0 & i %% 5 == 0) {
    cat(i, "fizzbuzz \n")
  } else if (i %% 3 == 0) {
    cat(i, "fizz \n")
  } else if (i %% 5 == 0) {
    cat(i, "buzz \n")
  } else {
    print(i)
  }
}