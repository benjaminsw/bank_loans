library(randomForest)
#list of all variables names
#"Customer.ID","Forename","Surname","Age","Gender","Years.at.address",
#"Employment.status","Country","Current.debt","Postcode","Income","Own.home",
#"CCJs","Loan.amount","Outcome","Outward.code","Postcode.area"
train.forest<-subset(data, select = c("Age","Gender",
                                      "Years.at.address","Employment.status","Country","Current.debt",
                                      "Income","Own.home","CCJs","Loan.amount","Outcome"))
set.seed(71)
iris.rf <- randomForest(Species ~ ., data=data, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)
importance(iris.rf) 
