#read data in
data<-read.csv("D:\\Box Sync\\StirUni\\ITNPBD6\\assignment\\loans.csv")
summary(data)
names(data)
#customer
summary(data$Customer.ID)
data$Customer.ID<-factor(data$Customer.ID)
table(data$Customer.ID)
#forname
summary(data$Forename)
head(table(data$Forename))
tail(table(data$Forename))
length(unique(data$Forename))
subset(data, Forename==".", select=c(Forename, Surname))
#Surname
summary(data$Surname)
head(table(data$Surname))
tail(table(data$Surname))
length(unique(data$Surname))
subset(data, Surname=="@", select=c(Forename, Surname))
#age
summary(data$Age)
var(data$Age)
#gender
summary(sort(data$Gender))
#Years.at.address
summary(data$Years.at.address)
tail(table(data$Years.at.address))
#Employment.status
summary(data$Employment.status)
#Country
summary(data$Country)
#Current.debt
summary(data$Current.debt)
table(data$Current.debt)
#Postcode
length(unique(data$Postcode))
#Income
summary(data$Income)
#Own.home
summary(data$Own.home)
#CCJs
tail(table(data$CCJs))
#Loan.amount
summary(data$Loan.amount)
#Outcome
summary(data$Outcome)
