library(tidyverse)
library(ggplot2)
library(MASS)
library(MPV)
library(stringi)
library(leaps)
library(tree)
library(ggpubr)

setwd("/Users/trucle/Desktop/archive-2")
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
