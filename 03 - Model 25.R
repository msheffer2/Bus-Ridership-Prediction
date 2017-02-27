# Set Working Directory
################################################################################
setwd("C:\\repos\\Bus-Ridership-Prediction")

# Load libraries
################################################################################
library(dplyr)
library(randomForest)
#Explicitly called = beepr, caret
source("fit_assess.R")

# Load Data
################################################################################
load("./data/dat_v1.Rdata")

train <- filter(dat_v1, tflag==1) 
train.r <- train$rides
train <- select(train, -tflag, -rides)

test <- filter(dat_v1, tflag==2)
test.r <- test$rides
test <- select(test, -tflag, -rides)

load("./data/test_cases.Rdata")
rm(dat_v1)

# Model
###############################################################################
num <- ncol(train) - 1

set.seed(2345)
fit <- randomForest(train.r ~ ., data=train, mtry=num, importance = TRUE)
rm(num)

#Predictions
p_train <- predict(fit, train)
p_test <- predict(fit, test)
p_case <- predict(fit, test_cases)

beepr::beep()

#Fit Assessment
###############################################################################
#NOTE: I had originally written a function called compile_output (not included 
#      in repo or referenced in any code) to compile all the outputs into a 
#      single Excel file to make evaluation easier.  The function fit_assess,
#      sources above and used below, is adapted from that function but only 
#      compiles the output for direct printing or saving.

fa<- fit_assess()
rm(fit_assess, train, test, test_cases, test.r, train.r)

#Overall RMSE
sprintf("MODEL RMSE:  Training = %.1f, Testing = %.1f", fa$train, fa$test)
sprintf("TEST CASE RMSE:  Historical = %.1f,  Model = %.1f", fa$hist, fa$case)

#Year RMSE
print(fa$year)

#Season RMSE
print(fa$season)

#Test Case Day of Week RMSE
print(fa$day)

#Test Case Line RMSE
print(fa$line)

#How many variables does it take to predict?
imp <- caret::varImp(fit)
nrow(imp)

mod25 <- list(fit=fit, p_train=p_train, p_test=p_test, p_case=p_case, imp=imp,
              assessment=fa)
save(mod25, file="./output/mod25.Rdata")
rm(mod25, fit, imp, fa, p_case, p_test, p_train)






