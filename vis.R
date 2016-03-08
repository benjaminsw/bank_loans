#visualisation
library(ggplot2)
#http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
library(wesanderson)
#age
ggplot(data, aes(Age))+stat_bin(aes(fill=..count..),breaks=seq(17,89, by=1))+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="Age",y="Count")+ggtitle("Age")

ggplot(data, aes(Age))+stat_bin(aes(fill=Outcome),breaks=seq(17,89, by=1))+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80),axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  ggtitle("Age")+ labs(x="Age",y="Count")+scale_fill_brewer(palette = "Purples")

#gender
table(data$Gender)
data$Gender <- factor(data$Gender, c("M","F","Male","Female","0","1","N","D","H"))

ggplot(data, aes(Gender,fill=..count..))+geom_bar()+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="Gender",y="Count")+ggtitle("Gender")

ggplot(data, aes(Gender,fill=Outcome))+geom_bar()+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="Gender",y="Count")+ggtitle("Gender")+scale_fill_brewer(palette = "Purples")

#Years.at.address
summary(data$Years.at.address)
###all
ggplot(data, aes(Years.at.address))+stat_bin(aes(fill=..count..),breaks=seq(1,561, by=1))+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="Years",y="Count")+ggtitle("The Number of Years at Current Address")
###less than 75
ggplot(data, aes(Years.at.address))+stat_bin(aes(fill=..count..),breaks=seq(1,75, by=1))+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="Years",y="Count")+ggtitle("The Number of Years at Current Address")

ggplot(data, aes(Years.at.address))+stat_bin(aes(fill=Outcome),breaks=seq(1,75, by=1))+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80),axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  ggtitle("The Number of Years at Current Address")+ labs(x="Years",y="Count")+scale_fill_brewer(palette = "Purples")

#Employment.status
ggplot(data, aes(Employment.status,fill=..count..))+geom_bar()+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="Employment status",y="Count")+ggtitle("Employment Status")

ggplot(data, aes(Employment.status,fill=Outcome))+geom_bar()+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="Employment status",y="Count")+ggtitle("Employment Status")+scale_fill_brewer(palette = "Purples")

#Country
ggplot(data, aes(Country,fill=..count..))+geom_bar()+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="Country",y="Count")+ggtitle("Country of Origin")

ggplot(data, aes(Country,fill=Outcome))+geom_bar()+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="Country",y="Count")+ggtitle("Country of Origin")+scale_fill_brewer(palette = "Purples")

#Current.debt
summary(data$Current.debt)
ggplot(data, aes(Current.debt))+stat_bin(aes(fill=..count..),breaks=seq(0,9980, by=500))+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="Current debt",y="Count")+ggtitle("Current Debt")

ggplot(data, aes(Current.debt))+stat_bin( aes(fill=Outcome),breaks=seq(0,9980, by=500))+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80),axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="Current debt",y="Count")+ggtitle("Current Debt")+scale_fill_brewer(palette = "Purples")

#Postcode
data$Postcode<-as.character(data$Postcode)
#Outward code
data$Outward.code<-sapply(strsplit(data$Postcode, " "), "[[", 1)
#Postcode area
data$Postcode.area<-gsub("[[:digit:]]","",data$Outward.code)

ggplot(data, aes(Postcode.area,fill=..count..))+geom_bar()+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=20), axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x="Postcode area",y="Count")+ggtitle("Postcode Area")

ggplot(data, aes(Postcode.area,fill=Outcome))+geom_bar()+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=20), axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x="Postcode area",y="Count")+ggtitle("Postcode Area")+scale_fill_brewer(palette = "Purples")

#Income
ggplot(data, aes(Income))+stat_bin(aes(fill=..count..),breaks=seq(3000,220000, by=5000))+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=40))+
  labs(x="Income",y="Count")+ggtitle("Income")

ggplot(data, aes(Income))+stat_bin(aes(fill=Outcome),breaks=seq(3000,220000, by=5000))+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=40))+
  labs(x="Income",y="Count")+ggtitle("Income")+scale_fill_brewer(palette = "Purples")

#Own.home
ggplot(data, aes(Own.home,fill=..count..))+geom_bar()+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="Own home",y="Count")+ggtitle("Own home")

ggplot(data, aes(Own.home,fill=Outcome))+geom_bar()+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="Own home",y="Count")+ggtitle("Own home")+scale_fill_brewer(palette = "Purples")

#CCJs
ggplot(data, aes(CCJs,fill=..count..))+geom_bar()+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="County Court Judgments",y="Count")+ggtitle("County Court Judgments")


#CCJs that less that 15
ggplot(subset(data, CCJs <15, select = CCJs), aes(CCJs,fill=..count..))+geom_bar()+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="County Court Judgments",y="Count")+ggtitle("County Court Judgments")

#Loan.amount
ggplot(data, aes(Loan.amount))+stat_bin(aes(fill=..count..),breaks=seq(13,54460,by=500))+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=40))+
  labs(x="Loan amount",y="Count")+ggtitle("Loan Amount")

ggplot(data, aes(Loan.amount))+stat_bin(aes(fill=Outcome),breaks=seq(13,54460,by=500))+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=40))+
  labs(x="Loan amount",y="Count")+ggtitle("Loan Amount")+scale_fill_brewer(palette = "Purples")

#Outcome
ggplot(data, aes(Outcome,fill=..count..))+geom_bar()+theme_dark()+
  theme(plot.title=element_text(face="bold",size=80), axis.title = element_text(face="bold", size=80),text = element_text(size=50))+
  labs(x="Outcome",y="Count")+ggtitle("Outcome")

ggplot(data, aes(Outcome,fill=..count..))+geom_bar(colour="black")+
  ggtitle("Outcome")+theme(plot.title=element_text(lineheight=10.8,color="#666666",face="bold",size=32))+
  labs(x="Outcome",y="Count")+theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22))

