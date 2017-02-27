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

load("./output/mod25.Rdata")

reference <- sprintf("MODEL 25:  Training = %.1f, Testing = %.1f, Hist. Cases = %.1f,  Model Cases = %.1f", 
                     mod25$assessment[[1]], mod25$assessment[[2]], mod25$assessment[[3]], mod25$assessment[[4]])

imp <- mod25$imp %>%
  tibble::rownames_to_column() %>%
  rename(Variable=rowname) %>%
  arrange(desc(Overall)) %>%
  filter(row_number() <= 50)
print(imp)
rm(mod25, imp)

#Saving out original files, just in case
safe_train <- train
safe_test <- test

#Saving out year data because fit_assess needs this variable to work
year_train <- train$year
year_test <- test$year

# Attempt #1 Pulling out 26 Important Variables
###############################################################################

keep <- c(
  "day", "season", "line",
  "frequent", "sum_black", "sum_nhpi", "sum_pci", "avgwalk", "sum_white", 
  "jobs_retail", "minwalk", "avg_nhpi", "jobs_nonretail", "avg_asian", 
  "crime_burglary", "sum_aian", "avg_college", "avg_hispanic", 
  "crime_homicide", "sum_vacant", "linepop", "avg_pci", "avg_white", 
  "crime_aggass", "wtd_ci"
)

#drop the line flags first
train <- select(train, -contains("line_"))
test <- select(test, -contains("line_"))

train <- train[keep]
test <- test[keep]

num <- ncol(train) - 1
set.seed(2345)
fit <- randomForest(train.r ~ ., data=train, mtry=num, importance = TRUE)
beepr::beep()

#Predictions
p_train <- predict(fit, train)
p_test <- predict(fit, test)
p_case <- predict(fit, test_cases)

#Putting year back in so that fit_assess will work properly
train <- cbind(train, year=year_train)
test <- cbind(test, year=year_test)

fa<- fit_assess()

#Overall RMSE
ref1 <- sprintf("MODEL R1:  Training = %.1f, Testing = %.1f, Hist. Cases = %.1f,  Model Cases = %.1f", 
        fa$train[[1]], fa$test[[1]], fa$hist[[1]], fa$case[[1]])

reference
ref1

r1 <- list(fit=fit, p_train=p_train, p_test=p_test, p_case=p_case, assessment=fa)
rm(fit, p_train, p_test, p_case, fa,  keep, num)

# Attempt #2 Pulling out Top 10
###############################################################################
train <- safe_train
test <- safe_test 
keep <- c(
  "day", "season", "line",
  "frequent", "sum_black", "sum_nhpi", "sum_pci", "avgwalk", "sum_white", 
  "jobs_retail", "minwalk", "avg_nhpi", "jobs_nonretail", "avg_asian", 
  "crime_burglary", "sum_aian", "avg_college", "avg_hispanic", 
  "crime_homicide", "sum_vacant", "linepop", "avg_pci", "avg_white", 
  "crime_aggass", "wtd_ci"
)

#drop the line flags first
train <- select(train, -contains("line_"))
test <- select(test, -contains("line_"))

train <- train[keep]
test <- test[keep]

num <- ncol(train) - 1
set.seed(2345)
fit <- randomForest(train.r ~ ., data=train, mtry=num, importance = TRUE)
beepr::beep()

#Predictions
p_train <- predict(fit, train)
p_test <- predict(fit, test)
p_case <- predict(fit, test_cases)

#Putting year back in so that fit_assess will work properly
train <- cbind(train, year=year_train)
test <- cbind(test, year=year_test)

fa<- fit_assess()

#Overall RMSE
ref2 <- sprintf("MODEL R2:  Training = %.1f, Testing = %.1f, Hist. Cases = %.1f,  Model Cases = %.1f", 
        fa$train[[1]], fa$test[[1]], fa$hist[[1]], fa$case[[1]])

reference
ref2

r2 <- list(fit=fit, p_train=p_train, p_test=p_test, p_case=p_case, assessment=fa)
rm(fit, p_train, p_test, p_case, fa,  keep, num)

# Attempt #3 Pulling out 12 Variables
###############################################################################
train <- safe_train
test <- safe_test 
keep <- c(
  "day", "season", "line",
  "frequent", "sum_pci", "avgwalk", "jobs_retail", "linepop", "minwalk", 
  "jobs_nonretail", "sum_vacant", "wtd_ci"
)

#drop the line flags first
train <- select(train, -contains("line_"))
test <- select(test, -contains("line_"))

train <- train[keep]
test <- test[keep]

num <- ncol(train) - 1
set.seed(2345)
fit <- randomForest(train.r ~ ., data=train, mtry=num, importance = TRUE)
beepr::beep()

#Predictions
p_train <- predict(fit, train)
p_test <- predict(fit, test)
p_case <- predict(fit, test_cases)

#Putting year back in so that fit_assess will work properly
train <- cbind(train, year=year_train)
test <- cbind(test, year=year_test)

fa<- fit_assess()

#Overall RMSE
ref3 <- sprintf("MODEL R3:  Training = %.1f, Testing = %.1f, Hist. Cases = %.1f,  Model Cases = %.1f", 
        fa$train[[1]], fa$test[[1]], fa$hist[[1]], fa$case[[1]])

reference
ref3

r3 <- list(fit=fit, p_train=p_train, p_test=p_test, p_case=p_case, assessment=fa)
rm(fit, p_train, p_test, p_case, fa, keep, num)

rm(safe_test, safe_train, test, train, test_cases, test.r, train.r, year_test, year_train, fit_assess)

# Comparing the Revisions; Using the third revision as the updated model
###############################################################################

reference
ref1
ref2
ref3

rm(r1, r2, ref1, ref2, ref3, reference)
mod25_rev <- r3
save(mod25_rev, file="./output/mod25_rev.Rdata")
rm(r3, mod25_rev)














