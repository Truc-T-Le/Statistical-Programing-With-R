library(tidyverse)
library(ggplot2)
library(ggpubr)
library(MASS)
library(MPV)
library(glmtoolbox)
library(stringi)


setwd("/Users/trucle/Desktop/archive-2")
df <- read.csv("Admission_Predict_Ver1.1.csv", header = TRUE, sep = ",")
gradcafe <- read.csv("Statistics.csv", header = TRUE, sep = ",")



# Find and handle missing elements in the training set
missing_elements <- which(is.na(df), arr.ind = TRUE)
print(missing_elements)
df <- na.omit(df)
df <- df[,-1] 


# Data preprocessing 

names(df) <- c("GRE", "TOEFL", "Uni_Ranking", "SOP", "LOR", "GPA", "Research", "Prob_of_Admission")

names(gradcafe) <- c("Institution", "Subject", "Degree_type", "a_r",
                "communication", "Date" , "GPA", 
                "GRE_Verbal", "GRE_Quant", "GRE_Writing", "GRE_Subject", "status",
                "Date_posted", "Comments")


# drop unnecessary features
df2 <- gradcafe |> dplyr::select( -c("communication", "Date_posted", "Comments")) |>
  drop_na(GPA) |>
  mutate(Degree_type = str_replace_all(Degree_type, "\\(.+?\\)", "")) |> 
  filter(grepl('2019|2018', Date))

# transform CGPA to GPA 
# scale probability of admission by a scale of 10
df <- df |> 
  mutate(GPA = (GPA / 10) * 4) |>
  mutate(Prob_of_Admission = Prob_of_Admission * 100)

# changing ranking and research to factor variables
df$Uni_Ranking[df$Uni_Ranking == 5] = "Tier 1"
df$Uni_Ranking[df$Uni_Ranking == 4] = "Tier 2"
df$Uni_Ranking[df$Uni_Ranking == 3] = "Tier 3"
df$Uni_Ranking[df$Uni_Ranking == 2] = "Tier 4"
df$Uni_Ranking[df$Uni_Ranking == 1] = "Tier 5"

df$Uni_Ranking <- factor(df$Uni_Ranking, 
                               levels=c("Tier 5", "Tier 4", "Tier 3", "Tier 2", "Tier 1")) 


df$Research[df$Research == 1] = "Yes"
df$Research[df$Research == 0] = "No"
df$Research <- factor(df$Research, levels=c("Yes", "No")) 


# Combined subjects into common field 

bio <- df2 |>
  filter(grepl('Bio|Genom|Med|Psycho|Measure|Social|Pharmaceutical', Subject))


data <- df2 |>
  filter(grepl('Data|Machine|Comp', Subject))

finance <- df2 |> 
  filter(grepl('Quant|Finan|Busin|Operation|Econo', Subject))

educ <- df2 |> 
  filter(grepl('Educ', Subject))

stat <- df2 |>
  filter(!grepl('Educ|Quant|Finan|Busin|Operation|Econo|Data|Machine|Comp|Psycho|Measure|Social|Bio|Genom|Med', Subject))


df2$Subject <- replace(df2$Subject, df2$Subject %in% as.list(stat$Subject), as.list(rep("Statistics", length(stat$Subject))))

df2$Subject <- replace(df2$Subject, df2$Subject %in% as.list(bio$Subject), as.list(rep("Biostatistics", length(bio$Subject))))

df2$Subject <- replace(df2$Subject, df2$Subject %in% as.list(finance$Subject), as.list(rep("Finance", length(finance$Subject))))

df2$Subject <- replace(df2$Subject, df2$Subject %in% as.list(data$Subject), as.list(rep("Data Science", length(data$Subject))))

df2$Subject <- replace(df2$Subject, df2$Subject %in% as.list(educ$Subject), as.list(rep("Education", length(educ$Subject))))



# Clean Institution Name If Possible
UCLA <- df2 |>
  filter(grepl('UCLA|Angeles', Institution))


df2$Institution <- replace(df2$Institution, df2$Institution %in% as.list(UCLA$Institution), as.list(rep("UCLA", length(UCLA$Institution))))

Berkeley <- df2 |>
  filter(grepl('UCB|Berkeley', Institution))

df2$Institution <- replace(df2$Institution, df2$Institution %in% as.list(Berkeley$Institution), as.list(rep("Berkeley", length(Berkeley$Institution))))

Davis <- df2 |>
  filter(grepl('UCD|Davis', Institution))

df2$Institution <- replace(df2$Institution, df2$Institution %in% as.list(Davis$Institution), as.list(rep("Davis", length(Davis$Institution))))

UCSC <- df2 |>
  filter(grepl('UCSC|Cruz', Institution))

df2$Institution <- replace(df2$Institution, df2$Institution %in% as.list(UCSC$Institution), as.list(rep("UCSC", length(UCSC$Institution))))

UCSD <- df2 |>
  filter(grepl('UCSD|University Of California, San Diego', Institution))

df2$Institution <- replace(df2$Institution, df2$Institution %in% as.list(UCSD$Institution), as.list(rep("UCSD", length(UCSD$Institution))))

UCI <- df2 |>
  filter(grepl('Irvine|UCI', Institution))

df2$Institution <- replace(df2$Institution, df2$Institution %in% as.list(UCI$Institution), as.list(rep("Irvine", length(UCI$Institution))))

UCSB <- df2 |>
  filter(grepl('Santa|UCSB', Institution))

df2$Institution <- replace(df2$Institution, df2$Institution %in% as.list(UCSB$Institution), as.list(rep("UCSB", length(UCSB$Institution))))

UCR <- df2 |>
  filter(grepl('UCR|Riverside', Institution))

df2$Institution <- replace(df2$Institution, df2$Institution %in% as.list(UCR$Institution), as.list(rep("UCR", length(UCR$Institution))))

stanford <- df2 |>
  filter(grepl('Stanford', Institution))

df2$Institution <- replace(df2$Institution, df2$Institution %in% as.list(stanford$Institution), as.list(rep("Stanford", length(stanford$Institution))))

USC <- df2 |>
  filter(grepl('Southern California|usc', Institution))

df2$Institution <- replace(df2$Institution, df2$Institution %in% as.list(USC$Institution), as.list(rep("USC", length(USC$Institution))))

SDSU <- df2 |>
  filter(grepl('San Diego State|SDSU', Institution))

df2$Institution <- replace(df2$Institution, df2$Institution %in% as.list(SDSU$Institution), as.list(rep("SDSU", length(SDSU$Institution))))


# Schools Interested In 
cali_schools <- df2|> filter(grepl('UCLA|Berekely|Davis|UCSD|UCSB|Stanford|UCI|UCR|UCSC|USC|SDSU|Caltech', Institution))
top_10 <-  df2|> filter(grepl('Harvard|Mellon|UCB|Berekely|Chicago|Columbia|Duke|Michigan|Penn|Stanford|Washington', Institution))


master_cali <- cali_schools |> filter(Degree_type == "Masters ") |> filter(a_r %in% c("Accepted", "Rejected")) |>
  dplyr::select( -c("Subject", "Date", "status")) |>
  group_by(Institution, a_r) |>
  mutate(Institution_frequency = n()) |>
  ungroup()

phd_cali <- cali_schools |> filter(Degree_type == "PhD ") |> filter(a_r %in% c("Accepted", "Rejected")) |>
  dplyr::select( -c("Subject", "Date", "status")) |>
  group_by(Institution, a_r) |>
  mutate(Institution_frequency = n()) |>
  ungroup()

total_phd <- df2 |> filter(Degree_type == "PhD ") |> filter(a_r %in% c("Accepted", "Rejected")) |>
  dplyr::select( -c("Subject", "Date", "status")) |>
  group_by(Institution, a_r) |>
  mutate(Institution_frequency = n()) |>
  ungroup()
total_phd$GPA[35] <- (total_phd$GPA[35]/10) * 4


total_master <- df2 |> filter(Degree_type == "Masters ") |> filter(a_r %in% c("Accepted", "Rejected")) |>
  dplyr::select( -c("Subject", "Date", "status")) |>
  group_by(Institution, a_r) |>
  mutate(Institution_frequency = n()) |>
  ungroup()

master_cali |> group_by(a_r) |> dplyr::summarize(GPA_Avg = mean(GPA, na.rm=TRUE))

phd_cali |> group_by(a_r) |> dplyr::summarize(GPA_Avg = mean(GPA, na.rm=TRUE))

ggplot(data = master_cali, aes(GPA, fill = as.factor(a_r))) +     
  geom_histogram(bins = 50, boundary = 0) +                  
  labs(title = "Graduate School Status by GPA (Master Program)",                        
       subtitle = "Grouped by Appplication Status") +
  xlab("GPA") +                                                          
  ylab("Frequency of Students") +                                               
  facet_grid(a_r ~ .) +                                                    
  theme_bw() +                                                                  
  scale_fill_manual(name = "Application Status",                                          
                    labels= c("Accepted (AVG GPA:3.68)", "Rejected (AVG GPA:3.73)"), 
                    values = c("Accepted" = "#6e304b", "Rejected" = "#e2ae6c")) +          
  theme(text=element_text(size=12,  family="Times New Roman"),                 
        strip.background = element_blank(), 
        strip.text = element_blank())


ggplot(data = phd_cali, aes(GPA, fill = as.factor(a_r))) +     
  geom_histogram(bins = 50, boundary = 0) +                  
  labs(title = "Graduate School Status by GPA (PhD Program)",                        
       subtitle = "Grouped by Appplication Status") +
  xlab("GPA") +                                                          
  ylab("Frequency of Students") +                                               
  facet_grid(a_r ~ .) +                                                    
  theme_bw() +                                                                  
  scale_fill_manual(name = "Application Status",                                          
                    labels= c("Accepted (AVG GPA:3.84)", "Rejected (AVG GPA:3.73)"), 
                    values = c("Accepted" = "#6e304b", "Rejected" = "#e2ae6c")) +          
  theme(text=element_text(size=12,  family="Times New Roman"),                 
        strip.background = element_blank(), 
        strip.text = element_blank())

total_phd |> group_by(a_r) |> dplyr::summarize(GPA_Avg = mean(GPA, na.rm=TRUE))

ggplot(data = total_phd, aes(GPA, fill = as.factor(a_r))) +     
  geom_histogram(bins = 50, boundary = 0) +                  
  labs(title = "Graduate School Status by GPA (PhD Program)",                        
       subtitle = "Grouped by Appplication Status") +
  xlab("GPA") +                                                          
  ylab("Frequency of Students") +                                               
  facet_grid(a_r ~ .) +                                                    
  theme_bw() +                                                                  
  scale_fill_manual(name = "Application Status",                                          
                    labels= c("Accepted (AVG GPA:3.82)", "Rejected (AVG GPA:3.76)"), 
                    values = c("Accepted" = "#6e304b", "Rejected" = "#e2ae6c")) +          
  theme(text=element_text(size=12,  family="Times New Roman"),                 
        strip.background = element_blank(), 
        strip.text = element_blank())


#Scatterplot for Quantitative Variables

quant_plot <- function(df, var_x, var_y, col, xlab, title_lab) {
  ggplot(data = df, 
         aes(x = var_x, y = var_y , color = col)) +
    geom_point(alpha = 0.5) +
    labs(x = xlab,
         y = "Chance of Admission (%)",
         title = title_lab) +
    theme(legend.title=element_text(size=10)) +
    guides(colour = guide_legend(reverse=T)) + 
    labs(color = "University \n Ranking") +
    theme_bw()
}

gre <- quant_plot(df, df$GRE, df$Prob_of_Admission, df$Uni_Ranking,
           "GRE Score", 
           "Chance of Admission VS. GRE Score") +
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8))

toefl <- quant_plot(df, df$TOEFL, df$Prob_of_Admission, df$Uni_Ranking,
           "TOEFFL Score",
           "Chance of Admission VS. TOEFL Score") +
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8))

gpa <- quant_plot(df, df$GPA, df$Prob_of_Admission, df$Uni_Ranking,
           "GPA (4.0 Scale)",
           "Chance of Admission VS. GPA") +
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8))


quant_plot(df, df$TOEFL, df$Prob_of_Admission, FALSE,
           "TOEFFL Score",
           "Chance of Admission VS. TOEFL Score") + theme(legend.position = "none")


ggarrange(
  gre, toefl, gpa,   
  common.legend = TRUE, legend = "right",
  nrow = 3, ncol = 1) 


SOP <- quant_plot(df, df$SOP, df$Prob_of_Admission, FALSE,
           "Statement of Purpose (Strength)",
           "") + theme(legend.position = "none") +
  theme(
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8))

LOR <- quant_plot(df, df$LOR, df$Prob_of_Admission, FALSE,
                  "Letter of Recomendation (Strength)",
                  "") + theme(legend.position = "none") +
  geom_point(color='black', alpha = 0.5) +
  theme(
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 8)) 

rank <- quant_plot(df, df$Uni_Ranking, df$Prob_of_Admission, FALSE,
                  "Undergraduate University Ranking (Tier)",
                  "") + theme(legend.position = "none") +
  geom_point(color='blue', alpha = 0.5) +
  theme(
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 8)) 


ggarrange(
  SOP, LOR,   
  nrow = 1, ncol = 2) 

box_plot <- function(df, x_var, y_var, title, x_lab, col) {
  ggplot(data = df,
    aes(x_var, y_var)) +
    geom_boxplot( color = col) +
    ggtitle(title) +
    theme_bw() +
    xlab(x_lab) +
    labs(y=NULL)
}


sop_box <- box_plot(df, as.factor(df$SOP), df$Prob_of_Admission,
         '',
         'Statement of Purpose (strength)',
         'red') +
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 8)) 

LOR_box <- box_plot(df, as.factor(df$LOR), df$Prob_of_Admission,
                    '',
                    'Letter of Recommendation (strength)',
                    'black') +
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 8))
  

rank_box <- box_plot(df, as.factor(df$Uni_Ranking), df$Prob_of_Admission,
                    '',
                    "Undergraduate University Ranking (Tier)",
                    'blue') +
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 8)) 

ggarrange(
  SOP, sop_box, LOR, LOR_box, rank, rank_box,    
  nrow = 3, ncol = 2, labels = c('A', "",
                                 'B', "",
                                 'C', "")) 

ggplot(data = df, aes(Prob_of_Admission, fill = Research)) +     
  geom_histogram(bins = 50, boundary = 0) +                  
  labs(title = "Chance of Admission to Graduate School Group By Research Experience") +
  xlab("Chance of Admission (%)")  +                                                          
  ylab("Frequency of Students") +                                               
  facet_grid(Research ~ .) +                                                    
  theme_bw() +                                                                  
  scale_fill_discrete(name = "Research", labels = c("Yes", "No"))     


ggplot(data = df, aes(x = GPA, y = Prob_of_Admission, color = Research)) +
  geom_point() +
  facet_grid(Research ~ .) +
  ylab("Chance of Admission (%)") +                                                          
  xlab("Overall GPA") + 
  labs(title = "Chance of Admission to Graduate School By Overall GPA",                        
       subtitle = "Grouped by Undergraduate Research Experience") +
  scale_fill_discrete(name = "Research", labels = c("Yes", "No"))



# Scatterplot for raw model

research_yes <- rep(0, 500)
research_no <- rep(0, 500)
research_yes[df$Research == "Yes"] <- 1
research_no[df$Research == "No"] <- 1
df <- cbind(df, research_yes,research_no)
df <- df[,-c(13, 14)]
df$Uni_Ranking_fac <- as.factor(df$Uni_Ranking)
contrasts(df$Uni_Ranking_fac) <- contr.treatment(n = 5)

df$Research_fac <- as.factor(df$Research)
contrasts(df$Research_fac) <- contr.treatment(n = 2)

fit <- lm(I(exp(Prob_of_Admission)) ~ GPA + LOR + as.numeric(SOP) + GRE + TOEFL + Research_fac + factor(Uni_Ranking), df)
fit <- lm(I(exp(Prob_of_Admission)) ~ ., df)
fit <- lm(I(exp(Prob_of_Admission)) ~ I(log(GRE)) + I(log(TOEFL)) + I(log(GPA)) + Research, df)
plot(fitted.values(fit), rstandard(fit), ylab = "Standarsized Residuals", main = "Residuals vs. Fitted Values for Raw Full Model")
abline(h = 0, col = "red")

fit <- lm(I(Prob_of_Admission^2) ~ GRE + TOEFL + SOP + LOR + GPA + research_yes + research_no + Uni_Ranking, data = df)
boxcox(fit, lambda = seq(1, 3, 1/10), data = df)

qqnorm(rstandard(fit), pch = 1, frame = FALSE, 
       main = "QQ-Plot of Residuals for Y")
qqline(rstandard(fit), col = "steelblue", lwd = 2)
