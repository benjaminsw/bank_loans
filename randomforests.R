#sources:
#http://h2o-release.s3.amazonaws.com/h2o/master/1247/docs-website/datascience/rf.html
#https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm
library(randomForest)
#set seed to make this reproducable
set.seed(99)
#change Postcode.area back to factor
data$Postcode.area <- as.factor(data$Postcode.area)
#remove levels which do not have counts
data$Gender <- as.factor(as.character(data$Gender))
#split data for train/test
## 75% of the sample size
sample.size <- floor(0.75 * nrow(data))
##select indexes of row for training set
train.idx <- sample(seq_len(nrow(data)), size=sample.size)

#-------------------------------------list of all variables names--------------------------------
#"Customer.ID","Forename","Surname","Age","Gender","Years.at.address",
#"Employment.status","Country","Current.debt","Postcode","Income","Own.home",
#"CCJs","Loan.amount","Outcome","Outward.code","Postcode.area"
#------------------------------------prep data for 1st randomforest----------------------------------
#selecting and assigning data point for traing set and test set
train.forest <- data[train.idx, c("Gender",
                                   "Years.at.address","Employment.status","Country","Current.debt",
                                   "Own.home","CCJs")]
train.outcome.forest <- data[train.idx, c("Outcome")]
test.forest <- data[-train.idx, c("Gender",
                                  "Years.at.address","Employment.status","Country","Current.debt",
                                  "Own.home","CCJs")]
test.outcome.forest <-data[-train.idx, c("Outcome")]
#--------------------------------------run 1st randomforest----------------------------------------------
loan.forest1<-randomForest(x=train.forest, y=train.outcome.forest ,
                           xtest=test.forest, ytest=outcome.forest, replace = TRUE, importance = TRUE)
loan.forest1$confusion
#loan.forest1$importance
#print(loan.forest1)
importance(loan.forest1) 
#------------------------------------prep data for 2nd forest---------------------------------------
#bining countinuous variables
#bin = 10
#Age
data$Age.bin10 <- as.numeric(cut_number(data$Age,10))
#Current.debt
data$Current.debt.bin10 <- as.numeric(cut_number(data$Current.debt,10))
#Income
data$Income.bin10 <- as.numeric(cut_number(data$Income,10))
#Loan.amount
data$Loan.amount.bin10 <- as.numeric(cut_number(data$Loan.amount,10))
#-------------------------------------run 2nd randomforest------------------------------------------
train.forest <- data[train.idx, c("Gender","Age.bin10","Current.debt.bin10","Income.bin10","Loan.amount.bin10",
                                  "Years.at.address","Employment.status","Country","Current.debt",
                                  "Own.home","CCJs")]
train.outcome.forest <- data[train.idx, c("Outcome")]
test.forest <- data[-train.idx, c("Gender","Age.bin10","Current.debt.bin10","Income.bin10","Loan.amount.bin10",
                                  "Years.at.address","Employment.status","Country","Current.debt",
                                  "Own.home","CCJs")]
test.outcome.forest <-data[-train.idx, c("Outcome")]

loan.forest2<-randomForest(x=train.forest, y=train.outcome.forest,
                           xtest=test.forest, ytest=outcome.forest, replace = TRUE, importance = TRUE)
loan.forest2$confusion
#loan.forest2$importance
#print(loan.forest2)
importance(loan.forest2) 
#------------------------------------prep data for 2nd forest---------------------------------------
#bining countinuous variables
#bin = 5
#Age
data$Age.bin5 <- as.numeric(cut_number(data$Age,5))
#Current.debt
data$Current.debt.bin5 <- as.numeric(cut_number(data$Current.debt,5))
#Income
data$Income.bin5 <- as.numeric(cut_number(data$Income,5))
#Loan.amount
data$Loan.amount.bin5 <- as.numeric(cut_number(data$Loan.amount,5))
#-------------------------------------run 3nd randomforest------------------------------------------
train.forest <- data[train.idx, c("Gender","Age.bin5","Current.debt.bin5","Income.bin5","Loan.amount.bin5",
                                  "Years.at.address","Employment.status","Country","Current.debt",
                                  "Own.home","CCJs")]
train.outcome.forest <- data[train.idx, c("Outcome")]
test.forest <- data[-train.idx, c("Gender","Age.bin5","Current.debt.bin5","Income.bin5","Loan.amount.bin5",
                                  "Years.at.address","Employment.status","Country","Current.debt",
                                  "Own.home","CCJs")]
test.outcome.forest <-data[-train.idx, c("Outcome")]

loan.forest3<-randomForest(x=train.forest, y=train.outcome.forest,
                           xtest=test.forest, ytest=outcome.forest, replace = TRUE, importance = TRUE)
loan.forest3$confusion
#loan.forest2$importance
#print(loan.forest2)
importance(loan.forest3) 