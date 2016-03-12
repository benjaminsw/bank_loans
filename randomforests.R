#sources:
#http://h2o-release.s3.amazonaws.com/h2o/master/1247/docs-website/datascience/rf.html
#https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm
library(randomForest)
#set seed to make this reproducable
set.seed(99)
#change Postcode.area back to factor
data$Postcode.area <- as.factor(data$Postcode.area)
#split data for train/test
## 75% of the sample size
sample.size <- floor(0.75 * nrow(data))
##select indexes of row for training set
train.idx <- sample(seq_len(nrow(data)), size=sample.size)
#selecting and assigning data point for traing set and test set
train.forest <- data[train.idx, c("Gender",
                                   "Years.at.address","Employment.status","Country","Current.debt",
                                   "Own.home","CCJs")]
train.outcome.forest <- data[train.idx, c("Outcome")]
test.forest <- data[-train.idx, c("Gender",
                                  "Years.at.address","Employment.status","Country","Current.debt",
                                  "Own.home","CCJs")]
test.outcome.forest <-data[-train.idx, c("Outcome")]
#list of all variables names
#"Customer.ID","Forename","Surname","Age","Gender","Years.at.address",
#"Employment.status","Country","Current.debt","Postcode","Income","Own.home",
#"CCJs","Loan.amount","Outcome","Outward.code","Postcode.area"
loan1.forest<-randomForest(x=train.forest, y=train.outcome.forest , data=train.forest,
                           xtest=test.forest, ytest=outcome.forest, replace = TRUE, importance = TRUE)
loan1.forest$confusion
loan1.forest$importance

iris.rf <- randomForest(Outcome ~ ., data=train.forest, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)
importance(iris.rf) 
