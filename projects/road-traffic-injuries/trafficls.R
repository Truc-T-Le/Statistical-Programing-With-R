library(tidyverse)
library(ggplot2)
library(dplyr)

setwd("/Users/trucle/Desktop/MATH167R/projects/road-traffic-injuries")
df <- read.csv("road-traffic-injuries-2002-2010.csv", header = TRUE, sep = ",")
county <- unique(df$county_name)
region <- unique(df$region_name)

#region

region_inj <- df |>  select("reportyear", "region_name", "severity", "injuries", "mode", "totalpop", "poprate") |>
  filter(severity == "Severe Injury") |> 
  filter(reportyear %in% c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010")) |>
  filter(mode !='All modes' ) |>
  drop_na() |>
  mutate_at(c('reportyear', 'injuries'), as.numeric)
  


total_region_inj <- region_inj %>% group_by(reportyear, region_name) %>% 
  summarise(sum_injuries = sum(injuries),
            .groups = 'drop') |>
  filter(!row_number() %in% c(1, 16, 31, 46, 61, 76, 91, 106, 121)) 

ggplot(total_region_inj, aes(x = reportyear, y = sum_injuries, color = region_name )) +
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = total_region_inj$reportyear) +
  xlab("Year") +
  ylab("Injuries Counts") +
  ggtitle("Injury Counts Per Regions") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  theme(legend.title=element_blank())

x <- region_inj[c("reportyear", "region_name", "totalpop")] |> 
  group_by(reportyear, region_name, totalpop) |> 
  filter (! duplicated(totalpop)) |> 
  group_by(reportyear, region_name) |>
  summarise(Freq = sum(totalpop)) |>
  filter(!row_number() %in% c(1, 16, 31, 46, 61, 76, 91, 106, 121))
  

total_region_inj$rate_inj <- (total_region_inj$sum_injuries / x$Freq) * 100000

ggplot(total_region_inj, aes(x = reportyear, y = rate_inj, color = region_name )) +
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = total_region_inj$reportyear) +
  xlab("Year") +
  ylab("Injuries Counts") +
  ggtitle("Injury Counts Per Regions Per 100,000 People") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  theme(legend.title=element_blank())



top_inj <- total_region_inj %>% group_by(reportyear) %>% slice_max(rate_inj, n = 2) 


y <- region_inj |> filter(reportyear %in%  top_inj$reportyear, region_name %in% top_inj$region_name) |> 
  group_by(reportyear, region_name, mode) |>
  summarise(Freq = sum(injuries)) |>
  slice_max(Freq, n = 4)





region_cas <- df |>  select("reportyear", "region_name", "severity", "injuries", "mode", "totalpop", "poprate") |>
  filter(severity == "Killed") |> 
  filter(reportyear %in% c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010")) |>
  filter(mode !='All modes' ) |>
  drop_na() |>
  mutate_at(c('reportyear', 'injuries'), as.numeric)

total_region_cas <- region_cas %>% group_by(reportyear, region_name) %>% 
  summarise(sum_injuries = sum(injuries),
            .groups = 'drop') |>
  filter(!row_number() %in% c(1, 16, 31, 46, 61, 76, 91, 106, 121))

ggplot(total_region_cas, aes(x = as.numeric(reportyear), y = sum_injuries, color = region_name )) +
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = total_region_cas$reportyear) +
  xlab("Year") +
  ylab("Casualty Counts") +
  ggtitle("Casualty Counts Per Regions") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")


y <- region_cas[c("reportyear", "region_name", "totalpop")] |> 
  group_by(reportyear, region_name, totalpop) |> 
  filter (! duplicated(totalpop)) |> 
  group_by(reportyear, region_name) |>
  summarise(Freq = sum(totalpop)) |>
  filter(!row_number() %in% c(1, 16, 31, 46, 61, 76, 91, 106, 121))


total_region_cas$rate_inj <- (total_region_cas$sum_injuries / y$Freq) * 100000

ggplot(total_region_cas, aes(x = reportyear, y = rate_inj, color = region_name )) +
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = total_region_cas$reportyear) +
  xlab("Year") +
  ylab("Injuries Counts") +
  ggtitle("Injury Counts Per Regions Per 100,000 People") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  theme(legend.title=element_blank())


