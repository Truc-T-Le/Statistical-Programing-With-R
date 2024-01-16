library(tidyverse)
library(ggplot2)
library(dplyr)

setwd("/Users/trucle/Desktop/MATH167R/projects/road-traffic-injuries")
df <- read.csv("road-traffic-injuries-2002-2010.csv", header = TRUE, sep = ",")
county <- unique(df$county_name)
region <- unique(df$region_name)

region <- function(df, severe) {
region_num <- df |>  select("reportyear", "region_name", "severity", "injuries", "mode", "totalpop", "poprate") |>
    filter(severity == severe) |> 
    filter(reportyear %in% c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010")) |>
    filter(mode !='All modes' ) |>
    drop_na() |>
    mutate_at(c('reportyear', 'injuries'), as.numeric)
  
total_region_num <- region_num %>% group_by(reportyear, region_name) %>% 
    summarise(sum_injuries = sum(injuries),
              .groups = 'drop') |>
    filter(!row_number() %in% c(1, 16, 31, 46, 61, 76, 91, 106, 121)) 

freq <- region_num[c("reportyear", "region_name", "totalpop")] |> 
  group_by(reportyear, region_name, totalpop) |> 
  filter (! duplicated(totalpop)) |> 
  group_by(reportyear, region_name) |>
  summarise(Freq = sum(totalpop)) |>
  filter(!row_number() %in% c(1, 16, 31, 46, 61, 76, 91, 106, 121))

total_region_num$rate <- (total_region_num$sum_injuries / freq$Freq) * 100000
return(list(region_num, total_region_num))
}

region_graph <- function(df, cases, count_lab, title_lab) {
  ggplot(df, aes(x = reportyear, y = cases, color = region_name )) +
    geom_line() + 
    geom_point() +
    scale_x_continuous(breaks = df$reportyear) +
    xlab("Year") +
    ylab(count_lab) +
    ggtitle(title_lab) +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
    theme(legend.title=element_blank())
}

# Injuries by Regions
inj <- region(df, "Severe Injury")

region_graph(as.data.frame(inj[2]), as.data.frame(inj[2])$sum_injuries, "Injuries Counts", "Injury Counts Per Regions")

region_graph(as.data.frame(inj[2]), as.data.frame(inj[2])$rate, "Injuries Counts", "Injury Counts Per Regions Per 100,000 People")

top_inj <- as.data.frame(inj[2]) %>% group_by(reportyear) %>% slice_max(rate, n = 2) 
bottom_inj <- as.data.frame(inj[2]) %>% group_by(reportyear) %>% slice_min(rate, n = 2) 


# Casuality by Regions
cas <- region(df, "Killed")

region_graph(as.data.frame(cas[2]), as.data.frame(cas[2])$sum_injuries, "Casualty Counts", "Casualty Counts Per Regions")

region_graph(as.data.frame(cas[2]), as.data.frame(cas[2])$rate, "Casualty Counts", "Casualty Counts Per Regions Per 100,000 People")

top_cas <- as.data.frame(cas[2]) %>% group_by(reportyear) %>% slice_max(rate, n = 2) 
bottom_cas <- as.data.frame(cas[2]) %>% group_by(reportyear) %>% slice_min(rate, n = 2) 


