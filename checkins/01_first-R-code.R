sides <- c("H", "T")

flip1 <- sample(sides, 1, replace = TRUE)
print(flip1)

flip10 <- sample(sides, 10, replace = TRUE)
print(flip10)

flip100 <- sample(sides, 100, replace = TRUE)
print(flip100)