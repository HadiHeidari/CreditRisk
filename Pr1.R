library(ggplot2) # for some amazing looking graphs
library(MASS) # Library for our box-cox transform down the end
library(corrplot) # Plotting nice correlation matrix
library(cowplot) # arranging plots into a grid
library(dplyr) # Lirary for spliting train & test dataset
library(tidyverse) # Lirary for spliting train & test dataset
library("readxl")
library(pROC)

my_data <- read_excel("CRTEST.xlsx", sheet = 2)
my_data1 <- readxl::read_excel(path = "CRTEST.xlsx",sheet = 2, na = c("N/A", "n/a"))
str(my_data) 
summary(my_data)
colSums(is.na(my_data))

ggplot(my_data, aes(x = Loanvolume)) + geom_histogram(binwidth = 10) +
  geom_vline(xintercept = mean(my_data$Loanvolume), color = "indianred") +
  geom_vline(xintercept = median(my_data$Loanvolume), color = "cornflowerblue")

#na.rm = TRUE

model_glm = glm(Loan_Stat ~ Salary+ Loanvolume, data = my_data, family = "binomial")
coef(model_glm)

head(predict(model_glm, type = "response"))

test_prob = predict(model_glm, newdata = my_data, type = "response")
test_roc = roc(my_data$Loan_Stat ~ test_prob, plot = TRUE, print.auc = TRUE)







 