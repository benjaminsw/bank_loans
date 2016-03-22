#--------------------------------2 nodes in second layer------------------------------------- 
library(neuralnet)
set.seed(500)
#--------------------------------learning 0.1------------------------------------------------
##nn with learning 0.1 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(3,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes3.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes3.3)
table(test.nn$Outcome == test.nn$rate0.1nodes3.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes3.3)["FALSE"]
##nn with learning 0.1 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(4,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes4.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes4.3)
table(test.nn$Outcome == test.nn$rate0.1nodes4.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes4.3)["FALSE"]
##nn with learning 0.1 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(5,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes5.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes5.3)
table(test.nn$Outcome == test.nn$rate0.1nodes5.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes5.3)["FALSE"]
##nn with learning 0.1 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(6,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes6.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes6.3)
table(test.nn$Outcome == test.nn$rate0.1nodes6.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes6.3)["FALSE"]
##nn with learning 0.1 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(7,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes7.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes7.3)
table(test.nn$Outcome != test.nn$rate0.1nodes7.3)["TRUE"]
table(test.nn$Outcome != test.nn$rate0.1nodes7.3)["FALSE"]
##nn with learning 0.1 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(8,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes8.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes8.3)
table(test.nn$Outcome != test.nn$rate0.1nodes8.3)["TRUE"]
table(test.nn$Outcome != test.nn$rate0.1nodes8.3)["FALSE"]
##nn with learning 0.1 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(9,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes9.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes9.3)
table(test.nn$Outcome == test.nn$rate0.1nodes9.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes9.3)["FALSE"]
##nn with learning 0.1 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(10,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes10.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes10.3)
table(test.nn$Outcome == test.nn$rate0.1nodes10.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes10.3)["FALSE"]
#--------------------------------learning 0.03------------------------------------------------
##nn with learning 0.03 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(3,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes3.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes3.3)
table(test.nn$Outcome == test.nn$rate0.03nodes3.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes3.3)["FALSE"]
##nn with learning 0.03 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(4,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes4.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes4.3)
table(test.nn$Outcome == test.nn$rate0.03nodes4.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes4.3)["FALSE"]
##nn with learning 0.03 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(5,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes5.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes5.3)
table(test.nn$Outcome == test.nn$rate0.03nodes5.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes5.3)["FALSE"]
##nn with learning 0.03 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(6,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes6.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes6.3)
table(test.nn$Outcome == test.nn$rate0.03nodes6.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes6.3)["FALSE"]
##nn with learning 0.03 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(7,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes7.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes7.3)
table(test.nn$Outcome == test.nn$rate0.03nodes7.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes7.3)["FALSE"]
##nn with learning 0.03 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(8,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes8.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes8.3)
table(test.nn$Outcome == test.nn$rate0.03nodes8.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes8.3)["FALSE"]
##nn with learning 0.03 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(9,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes9.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes9.3)
table(test.nn$Outcome == test.nn$rate0.03nodes9.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes9.3)["FALSE"]
##nn with learning 0.03 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(10,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes10.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes10.3)
table(test.nn$Outcome == test.nn$rate0.03nodes10.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes10.3)["FALSE"]
#--------------------------------learning 0.01------------------------------------------------
##nn with learning 0.01 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(3,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes3.3<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes3.3)
table(test.nn$Outcome == test.nn$rate0.01nodes3.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes3.3)["FALSE"]
##nn with learning 0.01 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(4,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes4.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes4.3)
table(test.nn$Outcome == test.nn$rate0.01nodes4.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes4.3)["FALSE"]
##nn with learning 0.01 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(5,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes5.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes5.3)
table(test.nn$Outcome == test.nn$rate0.01nodes5.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes5.3)["FALSE"]
##nn with learning 0.01 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(6,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes6.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes6.3)
table(test.nn$Outcome == test.nn$rate0.01nodes6.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes6.3)["FALSE"]
##nn with learning 0.01 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(7,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes7.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes7.3)
table(test.nn$Outcome == test.nn$rate0.01nodes7.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes7.3)["FALSE"]
##nn with learning 0.01 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(8,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes8.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes8.3)
table(test.nn$Outcome == test.nn$rate0.01nodes8.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes8.3)["FALSE"]
##nn with learning 0.01 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(9,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes9.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes9.3)
table(test.nn$Outcome == test.nn$rate0.01nodes9.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes9.3)["FALSE"]
##nn with learning 0.01 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(10,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes10.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes10.3)
table(test.nn$Outcome == test.nn$rate0.01nodes10.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes10.3)["FALSE"]
#--------------------------------learning 0.003------------------------------------------------
##nn with learning 0.003 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(3,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes3.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes3.3)
table(test.nn$Outcome == test.nn$rate0.003nodes3.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes3.3)["FALSE"]
##nn with learning 0.003 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(4,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes4.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes4.3)
table(test.nn$Outcome == test.nn$rate0.003nodes4.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes4.3)["FALSE"]
##nn with learning 0.003 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(5,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes5.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes5.3)
table(test.nn$Outcome == test.nn$rate0.003nodes5.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes5.3)["FALSE"]
##nn with learning 0.003 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(6,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes6.3<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes6.3)
table(test.nn$Outcome == test.nn$rate0.003nodes6.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes6.3)["FALSE"]
##nn with learning 0.003 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(7,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes7.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes7.3)
table(test.nn$Outcome == test.nn$rate0.003nodes7.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes7.3)["FALSE"]
##nn with learning 0.003 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(8,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes8.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes8.3)
table(test.nn$Outcome == test.nn$rate0.003nodes8.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes8.3)["FALSE"]
##nn with learning 0.003 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(9,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes9.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes9.3)
table(test.nn$Outcome == test.nn$rate0.003nodes9.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes9.3)["FALSE"]
##nn with learning 0.003 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(10,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes10.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes10.3)
table(test.nn$Outcome == test.nn$rate0.003nodes10.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes10.3)["FALSE"]
#--------------------------------learning 0.01------------------------------------------------
##nn with learning 0.01 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(3,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes3.3<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes3.3)
table(test.nn$Outcome == test.nn$rate0.001nodes3.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes3.3)["FALSE"]
##nn with learning 0.001 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(4,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes4.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes4.3)
table(test.nn$Outcome == test.nn$rate0.001nodes4.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes4.3)["FALSE"]
##nn with learning 0.001 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(5,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes5.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes5.3)
table(test.nn$Outcome == test.nn$rate0.001nodes5.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes5.3)["FALSE"]
##nn with learning 0.001 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(6,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes6.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes6.3)
table(test.nn$Outcome == test.nn$rate0.001nodes6.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes6.3)["FALSE"]
##nn with learning 0.001 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(7,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes7.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes7.3)
table(test.nn$Outcome == test.nn$rate0.001nodes7.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes7.3)["FALSE"]
##nn with learning 0.001 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(8,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes8.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes8.3)
table(test.nn$Outcome == test.nn$rate0.001nodes8.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes8.3)["FALSE"]
##nn with learning 0.001 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(9,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes9.3<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes9.3)
table(test.nn$Outcome == test.nn$rate0.001nodes9.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes9.3)["FALSE"]
##nn with learning 0.001 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(10,3))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes10.3 <-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes10.3)
table(test.nn$Outcome == test.nn$rate0.001nodes10.3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes10.3)["FALSE"]



