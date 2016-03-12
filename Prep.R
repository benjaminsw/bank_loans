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







