#sources:
#http://h2o-release.s3.amazonaws.com/h2o/master/1247/docs-website/datascience/rf.html
#https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm
library(randomForest)
#set seed to make this reproducable
set.seed(99)
#split data for train/test
## 75% of the sample size
sample.size <- floor(0.75 * nrow(data))
##select indexes of row for training set
train.idx <- sample(seq_len(nrow(data)), size=sample.size)
#selecting and assigning data point for traing set and test set
train.forest <- data[train.idx, ]
test.forest <- data[-train.idx, ]
#list of all variables names
#"Customer.ID","Forename","Surname","Age","Gender","Years.at.address",
#"Employment.status","Country","Current.debt","Postcode","Income","Own.home",
#"CCJs","Loan.amount","Outcome","Outward.code","Postcode.area"
train.forest<-subset(train.forest, select = c("Gender",
                                      "Years.at.address","Employment.status","Country","Current.debt",
                                      "Own.home","CCJs","Postcode.area","Outcome"))

iris.rf <- randomForest(Outcome ~ ., data=train.forest, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)
importance(iris.rf) 
