#Sources:
#http://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
library(neuralnet)
set.seed(500)
#feature selection
numeric.data.nn <- data[,c("Age", "Years.at.address",           
                      "Current.debt", "Income", "CCJs", "Loan.amount")]
factor.data.nn <- data[,c("Gender", "Employment.status", "Country",           
                          "Postcode.area", "Own.home" , "Outcome")]
#factor to numeric
factor.data.nn <- as.data.frame(lapply(factor.data.nn,as.numeric))
#feature scaling
train.nn <- data.frame(numeric.data.nn, factor.data.nn)
maxs <- apply(train.nn, 2, max) 
mins <- apply(train.nn, 2, min)
train.nn.scaled <- as.data.frame(scale(train.nn, center = mins, scale = maxs - mins))
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train.nn <- train.nn.scaled[index,]
test.nn <- train.nn.scaled[-index,]

#prep nn
n <- names(train.nn)
f <- as.formula(paste("Outcome ~", paste(n[!n %in% "Outcome"], collapse = " + ")))
#--------------------------------learning 0.1------------------------------------------------
#nn with learning 0.1 hidden 1 ****** not converged
#test.nn <- subset(test.nn, select = -c(rate0.1nodes2))
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=1)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes1<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.1nodes1)
##nn with learning 0.1 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=2)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes2<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.1nodes2)
##nn with learning 0.1 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=3)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes3<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.1nodes3)
##nn with learning 0.1 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=4)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes4<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.1nodes4)
##nn with learning 0.1 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=5)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes5<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.1nodes5)
##nn with learning 0.1 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=6)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes6<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.1nodes6)
##nn with learning 0.1 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=7)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes7<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes7)
##nn with learning 0.1 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=8)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes8<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.1nodes8)
##nn with learning 0.1 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=9)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes9<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.1nodes9)
##nn with learning 0.1 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=10)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes10<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.1nodes10)
#--------------------------------learning 0.03------------------------------------------------
##nn with learning 0.03 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=2)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes2<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.03nodes2)
##nn with learning 0.03 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=3)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes3<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.03nodes3)
##nn with learning 0.03 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=4)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes4<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.03nodes4)
##nn with learning 0.03 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=5)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes5<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.03nodes5)
##nn with learning 0.03 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=6)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes6<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.03nodes6)
##nn with learning 0.03 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=7)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes7<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.03nodes7)
##nn with learning 0.03 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=8)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes8<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.03nodes8)
##nn with learning 0.03 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=9)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes9<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.03nodes9)
##nn with learning 0.03 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=10)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes10<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.03nodes10)
#--------------------------------learning 0.01------------------------------------------------
##nn with learning 0.01 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=2)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes2<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.01nodes2)
##nn with learning 0.01 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=3)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes3<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.01nodes3)
##nn with learning 0.01 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=4)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes4<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.01nodes4)
##nn with learning 0.01 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=5)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes5<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.01nodes5)
##nn with learning 0.01 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=6)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes6<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.01nodes6)
##nn with learning 0.01 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=7)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes7<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.01nodes7)
##nn with learning 0.01 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=8)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes8<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.01nodes8)
##nn with learning 0.01 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=9)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes9<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.01nodes9)
##nn with learning 0.01 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=10)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes10<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.01nodes10)
#--------------------------------learning 0.003------------------------------------------------
##nn with learning 0.03 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=2)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes2<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.003nodes2)
##nn with learning 0.003 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=3)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes3<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.003nodes3)
##nn with learning 0.003 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=4)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes4<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.003nodes4)
##nn with learning 0.003 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=5)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes5<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.003nodes5)
##nn with learning 0.003 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=6)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes6<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.003nodes6)
##nn with learning 0.003 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=7)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes7<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.003nodes7)
##nn with learning 0.003 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=8)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes8<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.003nodes8)
##nn with learning 0.003 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=9)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes9<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.003nodes9)
##nn with learning 0.003 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=10)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes10<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.003nodes10)
#--------------------------------learning 0.01------------------------------------------------
##nn with learning 0.001 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=2)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes2<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.001nodes2)
##nn with learning 0.01 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=3)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes3<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.001nodes3)
##nn with learning 0.001 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=4)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes4<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.001nodes4)
##nn with learning 0.001 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=5)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes5<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.001nodes5)
##nn with learning 0.001 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=6)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes6<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.001nodes6)
##nn with learning 0.001 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=7)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes7<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.001nodes7)
##nn with learning 0.001 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=8)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes8<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.001nodes8)
##nn with learning 0.001 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=9)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes9<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.001nodes9)
##nn with learning 0.001 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=10)
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes10<-ifelse(predict.nn$net.result>0.5,"paid","defaulted")
nn
table(test.nn$Outcome, test.nn$rate0.001nodes10)



