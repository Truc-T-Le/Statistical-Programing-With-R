state_population <- readr::read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/national/totals/nst-est2020.csv")

# code to print the data
library(knitr)
library(tidyverse)
library(tidyr)
kable(state_population, digits = 3, row.names = F) |>
  kableExtra::kable_styling("striped", full_width = T) |> 
  kableExtra::scroll_box(height = "560px")

US_pop <- state_population |>
  filter(SUMLEV == "010")

# code to print the data
kable(US_pop, digits = 3, row.names = F) |>
  kableExtra::kable_styling("striped", full_width = T)

plot_dat <- data.frame(
  Year = 2010:2020,
  Population = as.numeric(US_pop[, c(8:17, 19)])
)
head(plot_dat)
plot(plot_dat$Year, plot_dat$Population)


US_pop |> 
  select(-POPESTIMATE042020) |>
  pivot_longer(contains("POPESTIMATE"), 
               names_to = "Year",
               values_to = "Population") |>
  mutate(Year = as.numeric(stringr::str_sub(Year, start = 12))) |>
  ggplot(aes(x = Year, y = Population)) + 
  geom_line()

state_population |> 
  filter(SUMLEV == "020") |>
  select(-POPESTIMATE042020) |>
  pivot_longer(contains("POPESTIMATE"), 
               names_to = "Year",
               values_to = "Population") |>
  mutate(Year = as.numeric(stringr::str_sub(Year, start = 12))) |>
  ggplot(aes(x = Year, y = Population, color = NAME)) + 
  geom_line()


# Want all column except religion so we do !column
relig_income |>
  pivot_longer(!religion, names_to = "income", values_to = "count")


billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )

us_rent_income |> 
  pivot_wider(names_from = "variable",
              values_from = c(estimate, moe))


