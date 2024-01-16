absolute_error <- function(observed, predicted){
  return(abs(observed - predicted))
}

absolute_error(weather_forecasts$observed_temp, weather_forecasts$forecast_temp)

x <- 1:10
for (i in 1:length(x)) {
  x[i] <- sum(x[0:i])
}
x

fibonacci <- function(x){
  if ( x == 1 | x == 2) {
    return(x = 1)
  } else {
    return(fibonacci(x - 1) + fibonacci(x - 2))
  }
}
fibonacci(6)