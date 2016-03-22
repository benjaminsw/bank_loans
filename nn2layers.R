#--------------------------------2 nodes in second layer------------------------------------- 
library(neuralnet)
set.seed(500)
#--------------------------------learning 0.1------------------------------------------------
##nn with learning 0.1 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(2,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes2.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes2.2)
table(test.nn$Outcome == test.nn$rate0.1nodes2.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes2.2)["FALSE"]
##nn with learning 0.1 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(3,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes3.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes3.2)
table(test.nn$Outcome == test.nn$rate0.1nodes3.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes3.2)["FALSE"]
##nn with learning 0.1 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(4,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes4.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes4.2)
table(test.nn$Outcome == test.nn$rate0.1nodes4.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes4.2)["FALSE"]
##nn with learning 0.1 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(5,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes5.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes5.2)
table(test.nn$Outcome == test.nn$rate0.1nodes5.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes5.2)["FALSE"]
##nn with learning 0.1 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(6,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes6.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes6.2)
table(test.nn$Outcome == test.nn$rate0.1nodes6.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes6.2)["FALSE"]
##nn with learning 0.1 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(7,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes7.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes7.2)
table(test.nn$Outcome != test.nn$rate0.1nodes7.2)["TRUE"]
table(test.nn$Outcome != test.nn$rate0.1nodes7.2)["FALSE"]
##nn with learning 0.1 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(8,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes8.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes8.2)
table(test.nn$Outcome != test.nn$rate0.1nodes8.2)["TRUE"]
table(test.nn$Outcome != test.nn$rate0.1nodes8.2)["FALSE"]
##nn with learning 0.1 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(9,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes9.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes9.2)
table(test.nn$Outcome == test.nn$rate0.1nodes9.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes9.2)["FALSE"]
##nn with learning 0.1 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(10,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes10.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes10.2)
table(test.nn$Outcome == test.nn$rate0.1nodes10.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes10.2)["FALSE"]
#--------------------------------learning 0.03------------------------------------------------
##nn with learning 0.03 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(2,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes2.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes2.2)
table(test.nn$Outcome == test.nn$rate0.03nodes2.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes2.2)["FALSE"]
##nn with learning 0.03 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(3,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes3.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes3.2)
table(test.nn$Outcome == test.nn$rate0.03nodes3.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes3.2)["FALSE"]
##nn with learning 0.03 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(4,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes4.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes4.2)
table(test.nn$Outcome == test.nn$rate0.03nodes4.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes4.2)["FALSE"]
##nn with learning 0.03 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(5,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes5<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes5.2)
table(test.nn$Outcome == test.nn$rate0.03nodes5.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes5.2)["FALSE"]
##nn with learning 0.03 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(6,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes6.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes6.2)
table(test.nn$Outcome == test.nn$rate0.03nodes6.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes6.2)["FALSE"]
##nn with learning 0.03 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(7,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes7<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes7.2)
table(test.nn$Outcome == test.nn$rate0.03nodes7.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes7.2)["FALSE"]
##nn with learning 0.03 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(8,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes8.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes8.2)
table(test.nn$Outcome == test.nn$rate0.03nodes8.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes8.2)["FALSE"]
##nn with learning 0.03 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(9,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes9.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes9.2)
table(test.nn$Outcome == test.nn$rate0.03nodes9.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes9.2)["FALSE"]
##nn with learning 0.03 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(10,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes10.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes10.2)
table(test.nn$Outcome == test.nn$rate0.03nodes10.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes10.2)["FALSE"]
#--------------------------------learning 0.01------------------------------------------------
##nn with learning 0.01 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(2,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes2.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes2.2)
table(test.nn$Outcome == test.nn$rate0.01nodes2.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes2.2)["FALSE"]
##nn with learning 0.01 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(3,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes3.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes3.2)
table(test.nn$Outcome == test.nn$rate0.01nodes3.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes3.2)["FALSE"]
##nn with learning 0.01 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(4,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes4.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes4.2)
table(test.nn$Outcome == test.nn$rate0.01nodes4.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes4.2)["FALSE"]
##nn with learning 0.01 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(5,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes5.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes5.2)
table(test.nn$Outcome == test.nn$rate0.01nodes5.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes5.2)["FALSE"]
##nn with learning 0.01 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(6,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes6.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes6.2)
table(test.nn$Outcome == test.nn$rate0.01nodes6.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes6.2)["FALSE"]
##nn with learning 0.01 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(7,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes7.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes7.2)
table(test.nn$Outcome == test.nn$rate0.01nodes7.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes7.2)["FALSE"]
##nn with learning 0.01 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(8,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes8.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes8.2)
table(test.nn$Outcome == test.nn$rate0.01nodes8.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes8.2)["FALSE"]
##nn with learning 0.01 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(9,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes9.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes9.2)
table(test.nn$Outcome == test.nn$rate0.01nodes9.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes9.2)["FALSE"]
##nn with learning 0.01 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(10,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes10.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes10.2)
table(test.nn$Outcome == test.nn$rate0.01nodes10.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes10.2)["FALSE"]
#--------------------------------learning 0.003------------------------------------------------
##nn with learning 0.03 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(2,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes2.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes2.2)
table(test.nn$Outcome == test.nn$rate0.003nodes2.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes2.2)["FALSE"]
##nn with learning 0.003 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(3,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes3.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes3.2)
table(test.nn$Outcome == test.nn$rate0.003nodes3.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes3.2)["FALSE"]
##nn with learning 0.003 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(4,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes4.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes4.2)
table(test.nn$Outcome == test.nn$rate0.003nodes4.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes4.2)["FALSE"]
##nn with learning 0.003 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(5,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes5.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes5.2)
table(test.nn$Outcome == test.nn$rate0.003nodes5.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes5.2)["FALSE"]
##nn with learning 0.003 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(6,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes6.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes6.2)
table(test.nn$Outcome == test.nn$rate0.003nodes6.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes6.2)["FALSE"]
##nn with learning 0.003 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(7,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes7.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes7.2)
table(test.nn$Outcome == test.nn$rate0.003nodes7.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes7.2)["FALSE"]
##nn with learning 0.003 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(8,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes8.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes8.2)
table(test.nn$Outcome == test.nn$rate0.003nodes8.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes8.2)["FALSE"]
##nn with learning 0.003 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(9,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes9.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes9.2)
table(test.nn$Outcome == test.nn$rate0.003nodes9.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes9.2)["FALSE"]
##nn with learning 0.003 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(10,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes10.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes10.2)
table(test.nn$Outcome == test.nn$rate0.003nodes10.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes10.2)["FALSE"]
#--------------------------------learning 0.01------------------------------------------------
##nn with learning 0.001 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(2,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes2.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes2.2)
table(test.nn$Outcome == test.nn$rate0.001nodes2.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes2.2)["FALSE"]
##nn with learning 0.01 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(3,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes3.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes3.2)
table(test.nn$Outcome == test.nn$rate0.001nodes3.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes3.2)["FALSE"]
##nn with learning 0.001 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(4,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes4.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes4.2)
table(test.nn$Outcome == test.nn$rate0.001nodes4.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes4.2)["FALSE"]
##nn with learning 0.001 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(5,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes5.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes5.2)
table(test.nn$Outcome == test.nn$rate0.001nodes5.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes5.2)["FALSE"]
##nn with learning 0.001 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(6,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes6.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes6.2)
table(test.nn$Outcome == test.nn$rate0.001nodes6.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes6.2)["FALSE"]
##nn with learning 0.001 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(7,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes7.2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes7.2)
table(test.nn$Outcome == test.nn$rate0.001nodes7.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes7.2)["FALSE"]
##nn with learning 0.001 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(8,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes8.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes8.2)
table(test.nn$Outcome == test.nn$rate0.001nodes8.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes8.2)["FALSE"]
##nn with learning 0.001 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(9,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes9.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes9.2)
table(test.nn$Outcome == test.nn$rate0.001nodes9.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes9.2)["FALSE"]
##nn with learning 0.001 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(10,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes10.2 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes10.2)
table(test.nn$Outcome == test.nn$rate0.001nodes10.2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes10.2)["FALSE"]



