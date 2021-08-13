#   LOAD PACKAGES   #########################################################

#   Load packages with pacman
pacman::p_load(pacman, rpart, rpart.plot, readxl, e1071, pROC, DescTools, caret)

#   Set working directory
setwd("/Users/nickromero/Desktop/UW-MADISON-ENG/ENGINEERING-412/HW5/Resources")

#   PROBLEM 1  #########################################################

#   1.a and 1.b   -----------------------

dat.1 = as.data.frame(read_excel('Uni6p1.xlsx'))
m1 <- rpart(status ~., data = dat.1, parms = list(split = 'information'))
m1 <- naiveBayes(status ~., data = dat.1, parms = list(split = 'information'))
print(m1)
summary(m1)
rpart.plot(m1,extra = 1)

#   PROBLEM 2  #########################################################

car = as.data.frame(read_excel('Uni6p2.xlsx'))
model = naiveBayes(Stolen ~ Color + Type + Origin, data = car)
#   Use the 10th row as the test set
predict(model, newdata = car[10, ], type = "raw") #   No: 0.1818182 < Yes: 0.8181818 [YES]
#   predict(model, newdata = car[10, ]) #   Getting a factor(0) and not a 'YES' result

#   PROBLEM 3  #########################################################

#   dummy data set
predict <- sample(c(0, 1), 20, replace=T)
true <- sample(c(0, 1), 20, replace=T)

#   1s retrieved
retrieved <- sum(predict)

#   Precision = fraction of relevant items retrieved 
precision <- sum(predict & true) / retrieved

#   Recall = fraction of relevant items retrieved
recall <- sum(predict & true) / sum(true)

#   Compute F-Measure
f.measure <- 2 * precision * recall / (precision + recall)
f.measure

#   H = n/(1/x1 + 1/x2 + ...+ 1/xn)

#   Combine F-Measure equation and Harmonic Mean
#   Or do we run the Hmean on a data set(i.e., predict)?
Hmean(f.measure)
m <- matrix(runif(50), nrow = 10)
apply(m, 2, Hmean)

#   Compute Harmonic Mean
sapply(as.data.frame(m), Hmean)

#   PROBLEM 4  #########################################################

#   Class col 'P' = 1 and 'N' = 0 
label = c(1, 0, 1, 1, 0, 1, 0, 0, 0, 1)
#   Probability col vals
predict = c(0.95, 0.85, 0.78, 0.66, 0.60, 0.55, 0.53, 0.52, 0.51, 0.40)
#   Plot ROC Curve
plot(roc(label ~ predict))

#   PROBLEM 5  #########################################################

set.seed(1234)

#   Two Sample t-test

#   Mean.1
mean.1 <- c(31.0, 32.6, 20.7, 20.6, 31.0, 42.0, 34.7, 26.8, 21.5, 26.1)
#   Mean.2
mean.2 <- c(22.3, 19.6, 25.4, 20.6, 20.7, 21.4, 22.1, 28.4, 26.2, 35.0)
result <- t.test(mean.1, mean.2)
result

#   PROBLEM 6  #########################################################

#   Leave-One-Out Cross-Validation

#   Tuple, Class, Actual data frame
#   'Positive' = 1 and 'Negative' = 0 

one.nn <- data.frame(Tuple = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                     Class = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
                     Actual= c(1, 1, 0, 1, 1, 0, 0, 0, 0, 0))

three.nn <- data.frame(Tuple = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                       Class = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
                       Actual= c(1, 1, 0, 1, 1, 0, 0, 0, 0, 0))

#   specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")

#   fit a regression model and use LOOCV to evaluate performance
model.one.nn <- train(Tuple ~ Class + Actual, data = one.nn, method = "lm", trControl = ctrl)
model.three.nn <- train(Tuple ~ Class + Actual, data = three.nn, method = "lm", trControl = ctrl)

#   view summary of LOOCV               
print(model.one.nn)
print(model.three.nn)

#   CLEAN UP  #########################################################

#   Clean environment
rm(list = ls())

#   Uninstall packages
p_unload(all)

#   Remove plots
dev.off()  # do only if a plot exists

#   Clear the console
cat("\014")   # command shortcut ctrl+L

