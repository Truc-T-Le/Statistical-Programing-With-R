library(tidyverse)
library(ggplot2)
library(MASS)
library(MPV)
library(stringi)
library(leaps)
library(tree)
library(ggpubr)

#GradCafe Datase
setwd("/Users/trucle/Desktop/archive-2")
gradcafe <- read.csv("Statistics.csv", header = TRUE, sep = ",")

# Data preprocessing 

names(gradcafe) <- c("Institution", "Subject", "Degree_type", "a_r",
                     "communication", "Date" , "GPA", 
                     "GRE_Verbal", "GRE_Quant", "GRE_Writing", "GRE_Subject", "status",
                     "Date_posted", "Comments")

# Parse for data from 2012-2019 since the GRE changed in August of 2011
# the dataset ends in 2019
df2 <- gradcafe |> dplyr::select( -c("communication", "Date_posted")) |>
  drop_na(GPA) |>
  mutate(Degree_type = str_replace_all(Degree_type, "\\(.+?\\)", "")) |> 
  filter(grepl('2012|2013|2014|2015|2016|2017|2018|2019', Date))



master <- df2 |> 
  filter(a_r %in% c("Accepted", "Rejected")) |>
  filter(Degree_type == "Masters ")

phd <- df2 |> 
  filter(a_r %in% c("Accepted", "Rejected")) |>
  filter(Degree_type == "PhD ")

top_10 <- df2 |>
  filter(a_r %in% c("Accepted", "Rejected")) |>
  filter(grepl('Harvard|Mellon|UCB|Berekely|Chicago|Columbia|Duke|Michigan|Penn|Stanford|Washington', Institution)) |>
  filter(!grepl('state|State|Missouri|St. Louis|Loyola|George Washington|Western Michigan University|British|Michigan Technological University', Institution)) 

top_10_phd <- top_10 |>
  filter(Degree_type == "PhD ")
  
# scaling gpa to the 4.0 scale
phd$GPA[35] <- (8.56/10)*4
phd$GPA[500] <- 4.0

master$GPA[451] <- (9.99/10)*4
master$GPA[666] <- (9.99/10)*4
master$GPA[34] <- (8.56/10)*4


# Distribution of GPA for admission to PHD program from 2011-2019
ggplot(data = phd, aes(GPA, fill = as.factor(a_r))) +     
  geom_histogram(bins = 50, boundary = 0) +                  
  labs(title = "Graduate School Status by GPA (PhD Program)",                        
       subtitle = "Grouped by Appplication Status") +
  xlab("GPA") +                                                          
  ylab("Frequency of Students") +                                               
  facet_grid(a_r ~ .) +                                                    
  theme_bw() +                                                                  
  scale_fill_manual(name = "Application Status",                                          
                    labels= c(paste("Accepted GPA Median:", round(median(phd$GPA[phd$a_r == "Accepted"]), 3)),
                              paste("Rejected GPA Median:", round(median(phd$GPA[phd$a_r == "Rejected"]), 3))), 
                    values = c("Accepted" = "#6e304b", "Rejected" = "#e2ae6c")) +          
  theme(text=element_text(size=12,  family="Times New Roman"),                 
        strip.background = element_blank(), 
        strip.text = element_blank())


# Distribution of GPA for admission to Top 10 PHD program from 2011-2019
ggplot(data = top_10_phd, aes(GPA, fill = as.factor(a_r))) +     
  geom_histogram(bins = 50, boundary = 0) +                  
  labs(title = "Graduate School Status by GPA (Top 10 PhD Program)",                        
       subtitle = "Grouped by Appplication Status") +
  xlab("GPA") +                                                          
  ylab("Frequency of Students") +                                               
  facet_grid(a_r ~ .) +                                                    
  theme_bw() +                                                                  
  scale_fill_manual(name = "Application Status",                                          
                    labels= c(paste("Accepted GPA Median:", round(median(top_10_phd$GPA[top_10_phd$a_r == "Accepted"]), 3)),
                              paste("Rejected GPA Median:", round(median(top_10_phd$GPA[top_10_phd$a_r == "Rejected"]), 3))), 
                    values = c("Accepted" = "#6e304b", "Rejected" = "#e2ae6c")) +          
  theme(text=element_text(size=12,  family="Times New Roman"),                 
        strip.background = element_blank(), 
        strip.text = element_blank())

# Distribution of GPA for admission to master program from 2011-2019
ggplot(data = master, aes(GPA, fill = as.factor(a_r))) +     
  geom_histogram(bins = 50, boundary = 0) +                  
  labs(title = "Graduate School Status by GPA (Master Program)",                        
       subtitle = "Grouped by Appplication Status") +
  xlab("GPA") +                                                          
  ylab("Frequency of Students") +                                               
  facet_grid(a_r ~ .) +                                                    
  theme_bw() +                                                                  
  scale_fill_manual(name = "Application Status",                                          
                    labels= c(paste("Accepted GPA Median:", round(median(master$GPA[master$a_r == "Accepted"]), 3)),
                              paste("Rejected GPA Median:", round(median(master$GPA[master$a_r == "Rejected"]), 3))), 
                    values = c("Accepted" = "#6e304b", "Rejected" = "#e2ae6c")) +          
  theme(text=element_text(size=12,  family="Times New Roman"),                 
        strip.background = element_blank(), 
        strip.text = element_blank())

status_top_10 <- top_10_phd_acc |> filter(status %in% c("A", "I", "U"))

ggplot(data = status_top_10, aes(GPA, fill = as.factor(a_r))) +     
  geom_histogram(bins = 50, boundary = 0) +                  
  labs(title = "Graduate School Status by GPA (Master Program)",                        
       subtitle = "Grouped by Appplication Status") +
  xlab("GPA") +                                                          
  ylab("Frequency of Students") +                                               
  facet_grid(a_r ~ .) +                                                    
  theme_bw() +                                                                  
  scale_fill_manual(name = "Application Status",                                          
                    labels= c(paste("Accepted GPA Median:", round(median(master$GPA[master$a_r == "Accepted"]), 3)),
                              paste("Rejected GPA Median:", round(median(master$GPA[master$a_r == "Rejected"]), 3))), 
                    values = c("Accepted" = "#6e304b", "Rejected" = "#e2ae6c")) +          
  theme(text=element_text(size=12,  family="Times New Roman"),                 
        strip.background = element_blank(), 
        strip.text = element_blank())


# statistics summary of gpa and test scores for both master and phd
status_summary <- function(df, program, ar, sum_func) {
df |> 
    drop_na() |>
    filter(Degree_type == program) |>
    filter(a_r == ar) |>
    group_by(status) |>
    summarise(across(c(GPA, GRE_Verbal, GRE_Quant, GRE_Writing), sum_func))
} 

summary_n <- function(df, program, ar) {
  df |> 
    filter(Degree_type == program) |>
    filter(a_r == ar) |>
    group_by(status) |>
    tally()
}

master_10_acc <- cbind(status_summary(top_10, "Masters ", "Accepted", mean), summary_n(top_10, "Masters ", "Accepted"))
phd_10_acc <- cbind(status_summary(top_10, "PhD ", "Accepted", mean), summary_n(top_10, "PhD ", "Accepted"))

master_10_rej <- cbind(status_summary(top_10, "Masters ", "Rejected", mean), summary_n(top_10, "Masters ", "Rejected"))
phd_10_rej <- cbind(status_summary(top_10, "PhD ", "Rejected", mean), summary_n(top_10, "PhD ", "Rejected"))


master_acc <- cbind(status_summary(master, "Masters ", "Accepted", mean), summary_n(master, "Masters ", "Accepted"))
phd_acc <- cbind(status_summary(phd, "PhD ", "Accepted", mean), summary_n(phd, "PhD ", "Accepted"))

master_rej <- cbind(status_summary(master, "Masters ", "Rejected", mean), summary_n(master, "Masters ", "Rejected"))
phd_rej <- cbind(status_summary(phd, "PhD ", "Rejected", mean), summary_n(phd, "PhD ", "Rejected"))

# hypothesis test for research question 1 for gpa and test scores
hypo <- function(col_pred, col_stat) {
a_i <- t.test( col_pred[col_stat == "A"], col_pred[col_stat == "I"] )
a_u <-  t.test( col_pred[col_stat == "A"],  col_pred[col_stat == "U"] )
a_iu <- t.test( col_pred[col_stat == "A"],  col_pred[col_stat == c("I", "U")] )
i_u <-  t.test( col_pred[col_stat == "I"],  col_pred[col_stat == "U"] )
return(cbind(a_i, a_u, a_iu, i_u))
}

## gpa
master_gpa <- hypo(master$GPA, master$status)
phd_gpa <- hypo(phd$GPA, phd$status)

## gre verbal scores
master_verb <- hypo(master$GRE_Verbal, master$status)
phd_verb <- hypo(phd$GRE_Verbal, phd$status)

## gre quant scores
master_quant <- hypo(master$GRE_Quant, master$status)
phd_quant <- hypo(phd$GRE_Quant, phd$status)

## gre writing scores
master_write <- hypo(master$GRE_Writing, master$status)
phd_write <- hypo(phd$GRE_Writing, phd$status)

# Hypothesis testing for proportion
a_iu <- prop.test(x = c(82, 48),
          n = c(269, 114))
a_i <- prop.test(x = c(82, 70),
                 n = c(269, 194))


#Admission Dataset
df <- read.csv("Admission_Predict_Ver1.1.csv", header = TRUE, sep = ",")

# Find and handle missing elements in the training set
missing_elements <- which(is.na(df), arr.ind = TRUE)
print(missing_elements)
df <- na.omit(df)
df <- df[,-1] 


# Data preprocessing 

names(df) <- c("GRE", "TOEFL", "Uni_Ranking", "SOP", "LOR", "GPA", "Research", "Prob_of_Admission")


# transform CGPA to GPA 
# scale probability of admission by a scale of 10
df <- df |> 
  mutate(GPA = (GPA / 10) * 4) 

# changing ranking and research to factor variables
df$Uni_Ranking <- as.factor(df$Uni_Ranking) 
df$Research <- as.factor(df$Research) 

contrasts(df$Uni_Ranking) <- contr.treatment(n = 5, base = 1)
contrasts(df$Research) <- contr.treatment(n = 2, base = 1)

# histogram for the distribution of chance of admission
ggplot(df, aes(x=(Prob_of_Admission))) + 
  geom_histogram(color="black", fill="white") +
  labs(x = ("Probability of Admission (%)"),
       title = " Distribution for Probability of Admission") +
  theme(plot.title = element_text(size=15, hjust = 0.5))

# function for histogram
hist_plot <- function(df, pred, x_lab, main_title) {
  ggplot(df, aes(x= pred)) + 
    geom_histogram(color="black", fill="white") +
    labs(x = (x_lab),
         title = main_title) +
    theme(plot.title = element_text(size=15, hjust = 0.5))
} 

# histograms for the quantitative variables
hist_plot(df, df$SOP, "Statement of Purpose Score (Strength)", "Distribution of Statement of Purpose Scores")
hist_plot(df, df$LOR, "Letter of Recommendation Score (Strength)", "Distribution of Letter of Recommendation Scores")
hist_plot(df, df$GRE, "GRE Score ", "Distribution of GRE Scores")
hist_plot(df, df$TOEFL, "TOEFL Score ", "Distribution of TOEFL Scores")
hist_plot(df, df$GPA, "GPA", "Distribution of GPA")
hist_plot(df,(df$Prob_of_Admission)^2.5, "(Probability of Admission)^2 (%)", "Distribution of Transformed Probability of Admission")

scatter <- function(df, pred, resp, x_lab, y_lab, main_title) {
  ggplot(df, aes(x= pred, y =resp)) + 
    geom_point(alpha = 0.5) +
    labs(x = x_lab,
         y = y_lab,
         title = main_title) 
}

#scatterplots for quantitative varibles 
gre <- scatter(df, df$GRE, df$Prob_of_Admission,
               "GRE Score", "Probability of Admission (%)",
               "Admission Chance VS. GRE Score") +
  theme(plot.title = element_text(size=7, hjust = 0.5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8)) +
  geom_smooth(method='lm', colour = 'blue')

gpa <- scatter(df, df$GPA, df$Prob_of_Admission,
               "GPA (4.0 Scale)", "Probability of Admission (%)",
               "Admission Chance VS. GPA") +
  theme(plot.title = element_text(size=7, hjust = 0.5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8)) +
  geom_smooth(method='lm', colour = 'blue')

toefl <- scatter(df, df$TOEFL, df$Prob_of_Admission,
                 "TOEFL Score", "Probability of Admission (%)",
                 "Admission Chance VS. TOEFL Scores") +
  theme(plot.title = element_text(size=7, hjust = 0.5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8)) +
  geom_smooth(method='lm', colour = 'blue')

gre2 <- scatter(df, df$GRE, (df$Prob_of_Admission)^2,
                "GRE Score", expression("(Chance of Admission)"^2 ~ "(%)"),
                expression("(Chance of Admission)"^2~" VS. GRE Score"))+
  theme(plot.title = element_text(size=7, hjust = 0.5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8)) +
  geom_smooth(method='lm', colour = 'red')

gpa2 <- scatter(df, df$GPA, (df$Prob_of_Admission)^2,
                "GPA (4.0 Scale)", expression("(Chance of Admission)"^2 ~ "(%)"),
                expression("(Chance of Admission)"^2~" VS. GPA")) +
  theme(plot.title = element_text(size=7, hjust = 0.5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8)) +
  geom_smooth(method='lm', colour = 'red')

toefl2 <- scatter(df, df$TOEFL, (df$Prob_of_Admission)^2,
                  "TOEFL Score", expression("(Chance of Admission)"^2 ~ "(%)"),
                  expression("(Chance of Admission)"^2~" VS. TOEFL Score"))+
  theme(plot.title = element_text(size=7, hjust = 0.5),
        axis.title.y = element_text(size = 8),
        axis.title.x = element_text(size = 8)) +
  geom_smooth(method='lm', colour = 'red')

ggarrange(
  gre, gpa, toefl, gre2, gpa2, toefl2,   
  nrow = 2, ncol = 3) 

#split data into testing and training
set.seed(43)
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train  <- df[sample, ]
test   <- df[!sample, ]

#Fitting training data
fit <- lm(Prob_of_Admission ~  GRE + TOEFL + LOR + SOP + GPA + Research + Uni_Ranking, data = train)
boxcox(fit, lambda = seq(1, 5, 1/10), data = df)
plot(fit)

fit.1 <- lm(I(Prob_of_Admission^2) ~ GRE + TOEFL + LOR + SOP + GPA + Research + Uni_Ranking, data = train)
boxcox(fit.1, lambda = seq(1, 5, 1/10), data = df)


#backward variable selection
backward <- regsubsets(I(Prob_of_Admission^2) ~ ., data = train,
                       method = 'backward',
                       nvmax = 10)
summary(backward)
Cp <- summary(backward)$cp
AdjR2 <- summary(backward)$adjr2
SSRes <- summary(backward)$rss
R2 <- summary(backward)$rsq
Matrix <- summary(backward)$which
p <- apply(Matrix,1, sum)
MSRes <- SSRes/(499-p)
output1 <- cbind(p, Matrix, SSRes, R2, AdjR2, MSRes, Cp)
output1

#forward variable selection
forward <- regsubsets(I(Prob_of_Admission^2) ~ ., data = train,
                      method = 'forward',
                      nvmax = 10)
summary(forward)
Cp <- summary(forward)$cp
AdjR2 <- summary(forward)$adjr2
SSRes <- summary(forward)$rss
R2 <- summary(forward)$rsq
Matrix <- summary(forward)$which
p <- apply(Matrix,1, sum)
MSRes <- SSRes/(499-p)
output2 <- cbind(p, Matrix, SSRes, R2, AdjR2, MSRes, Cp)
output2

#exhaustive variable selection
all <- regsubsets(I(Prob_of_Admission^2) ~ ., data = train, nbest = 2)
summary(all)
Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which
p <- apply(Matrix,1, sum)
MSRes <- SSRes/(499-p)
output5 <- cbind(p, Matrix, SSRes, R2, AdjR2, MSRes, Cp)
output5

#Training and Testing Errors for Final Model
final_model <- lm(I(Prob_of_Admission^2) ~ GRE + TOEFL + LOR + GPA + Research, data = train)
summary(final_model)

yhat_train <- final_model$coefficients[1] + final_model$coefficients[2]*train$GRE + final_model$coefficients[3]*train$TOEFL +
  final_model$coefficients[4]*train$LOR + final_model$coefficients[5]*train$GPA + final_model$coefficients[6]*as.numeric(train$Research)
train_err <- mean((train$Prob_of_Admission - sqrt(yhat_train))^2)

yhat_test <- final_model$coefficients[1] + final_model$coefficients[2]*test$GRE + final_model$coefficients[3]*test$TOEFL +
  final_model$coefficients[4]*test$LOR + final_model$coefficients[5]*test$GPA + final_model$coefficients[6]*as.numeric(test$Research)
test_err <- mean((test$Prob_of_Admission - sqrt(yhat_test))^2)


# graphing predicted vs observed value
ggtrain <- ggplot(train, aes(x =  sqrt(yhat_train), y = train$Prob_of_Admission)) + geom_point() +
  labs(title = "Training Data",  x = "Observed Chance of Admission", y = "Predicted Chance of Admission")+ 
  labs(subtitle = paste("Training error:", round(train_err, 3))) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_smooth(method='lm', colour = 'red')

ggtest <- ggplot(test, aes(x = sqrt(yhat_test), y = test$Prob_of_Admission)) + geom_point() +
  labs(title = "Testing Data",  x = "Observed Chance of Admission", y = "Predicted Chance of Admission")+ 
  labs(subtitle = paste("Testing error:", round(test_err, 3))) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  geom_smooth(method='lm', colour = 'red')

ggarrange(ggtrain, ggtest)

