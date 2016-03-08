#gender
subset(data, Gender=="Male",)
data$Gender[data$Gender %in% "Male"]<-"M"
subset(data, Gender=="Female",)
data$Gender[data$Gender %in% "Female"]<-"F"
summary(data$Gender)
subset(data, Gender=="0",c("Forename", "Surname"))
data$Gender[data$Gender %in% "0"]<-"F"
subset(data, Gender=="1",c("Forename", "Surname"))
data <- data[!data$Gender %in% c("0","1","D","H","N"),]
summary(data$Gender)
#Years.at.address
data<-subset(data, Years.at.address<100,)
tail(table(data$Years.at.address))
#CCJs
data<-subset(data, CCJs<10,)
tail(table(data$CCJs))


#split data for train/test
## 75% of the sample size
sample.size <- floor(0.75 * nrow(data))
## set the seed to make your partition reproductible
set.seed(99)
##select indexes of row for training set
train.idx <- sample(seq_len(nrow(data)), size=sample.size)
#selecting and assigning data point for traing set and test set
train.forest <- mtcars[train.idx, ]
test.forest <- mtcars[-train.idx, ]

