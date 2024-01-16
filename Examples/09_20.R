library(tidyverse)
data("ChickWeight")

# time is change to columns and weight is the entries for each time 
pivot_wider(ChickWeight,
            names_from = "Time",
            values_from = "weight")
