#--------------------------------2 nodes in second layer------------------------------------- 
#--------------------------------learning 0.1------------------------------------------------
##nn with learning 0.1 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(2,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes2)
table(test.nn$Outcome == test.nn$rate0.1nodes2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes2)["FALSE"]
##nn with learning 0.1 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(3,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes3<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes3)
table(test.nn$Outcome == test.nn$rate0.1nodes3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes3)["FALSE"]
##nn with learning 0.1 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(4,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes4<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes4)
table(test.nn$Outcome == test.nn$rate0.1nodes4)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes4)["FALSE"]
##nn with learning 0.1 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(5,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes5<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes5)
table(test.nn$Outcome == test.nn$rate0.1nodes5)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes5)["FALSE"]
##nn with learning 0.1 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(6,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes6<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes6)
table(test.nn$Outcome == test.nn$rate0.1nodes6)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes6)["FALSE"]
##nn with learning 0.1 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(7,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes7<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes7)
table(test.nn$Outcome != test.nn$rate0.1nodes7)["TRUE"]
table(test.nn$Outcome != test.nn$rate0.1nodes7)["FALSE"]
##nn with learning 0.1 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(8,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes8<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes8)
table(test.nn$Outcome != test.nn$rate0.1nodes8)["TRUE"]
table(test.nn$Outcome != test.nn$rate0.1nodes8)["FALSE"]
##nn with learning 0.1 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(9,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes9<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes9)
table(test.nn$Outcome == test.nn$rate0.1nodes9)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes9)["FALSE"]
##nn with learning 0.1 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.1, hidden=c(10,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.1nodes10<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.1nodes10)
table(test.nn$Outcome == test.nn$rate0.1nodes10)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.1nodes10)["FALSE"]
#--------------------------------learning 0.03------------------------------------------------
##nn with learning 0.03 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(2,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes2)
table(test.nn$Outcome == test.nn$rate0.03nodes2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes2)["FALSE"]
##nn with learning 0.03 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(3,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes3<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes3)
table(test.nn$Outcome == test.nn$rate0.03nodes3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes3)["FALSE"]
##nn with learning 0.03 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(4,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes4<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes4)
table(test.nn$Outcome == test.nn$rate0.03nodes4)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes4)["FALSE"]
##nn with learning 0.03 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(5,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes5<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes5)
table(test.nn$Outcome == test.nn$rate0.03nodes5)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes5)["FALSE"]
##nn with learning 0.03 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(6,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes6<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes6)
table(test.nn$Outcome == test.nn$rate0.03nodes6)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes6)["FALSE"]
##nn with learning 0.03 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(7,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes7<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes7)
table(test.nn$Outcome == test.nn$rate0.03nodes7)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes7)["FALSE"]
##nn with learning 0.03 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(8,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes8<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes8)
table(test.nn$Outcome == test.nn$rate0.03nodes8)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes8)["FALSE"]
##nn with learning 0.03 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(9,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes9<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes9)
table(test.nn$Outcome == test.nn$rate0.03nodes9)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes9)["FALSE"]
##nn with learning 0.03 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.03, hidden=c(10,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.03nodes10<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.03nodes10)
table(test.nn$Outcome == test.nn$rate0.03nodes10)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.03nodes10)["FALSE"]
#--------------------------------learning 0.01------------------------------------------------
##nn with learning 0.01 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(2,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes2)
table(test.nn$Outcome == test.nn$rate0.01nodes2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes2)["FALSE"]
##nn with learning 0.01 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(3,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes3<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes3)
table(test.nn$Outcome == test.nn$rate0.01nodes3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes3)["FALSE"]
##nn with learning 0.01 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(4,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes4<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes4)
table(test.nn$Outcome == test.nn$rate0.01nodes4)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes4)["FALSE"]
##nn with learning 0.01 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(5,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes5<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes5)
table(test.nn$Outcome == test.nn$rate0.01nodes5)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes5)["FALSE"]
##nn with learning 0.01 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(6,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes6<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes6)
table(test.nn$Outcome == test.nn$rate0.01nodes6)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes6)["FALSE"]
##nn with learning 0.01 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(7,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes7<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes7)
table(test.nn$Outcome == test.nn$rate0.01nodes7)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes7)["FALSE"]
##nn with learning 0.01 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(8,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes8<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes8)
table(test.nn$Outcome == test.nn$rate0.01nodes8)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes8)["FALSE"]
##nn with learning 0.01 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(9,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes9<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes9)
table(test.nn$Outcome == test.nn$rate0.01nodes9)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes9)["FALSE"]
##nn with learning 0.01 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01, hidden=c(10,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.01nodes10<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.01nodes10)
table(test.nn$Outcome == test.nn$rate0.01nodes10)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.01nodes10)["FALSE"]
#--------------------------------learning 0.003------------------------------------------------
##nn with learning 0.03 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(2,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes2)
table(test.nn$Outcome == test.nn$rate0.003nodes2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes2)["FALSE"]
##nn with learning 0.003 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(3,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes3<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes3)
table(test.nn$Outcome == test.nn$rate0.003nodes3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes3)["FALSE"]
##nn with learning 0.003 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(4,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes4<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes4)
table(test.nn$Outcome == test.nn$rate0.003nodes4)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes4)["FALSE"]
##nn with learning 0.003 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(5,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes5<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes5)
table(test.nn$Outcome == test.nn$rate0.003nodes5)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes5)["FALSE"]
##nn with learning 0.003 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(6,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes6<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes6)
table(test.nn$Outcome == test.nn$rate0.003nodes6)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes6)["FALSE"]
##nn with learning 0.003 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(7,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes7<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes7)
table(test.nn$Outcome == test.nn$rate0.003nodes7)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes7)["FALSE"]
##nn with learning 0.003 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(8,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes8<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes8)
table(test.nn$Outcome == test.nn$rate0.003nodes8)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes8)["FALSE"]
##nn with learning 0.003 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(9,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes9<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes9)
table(test.nn$Outcome == test.nn$rate0.003nodes9)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes9)["FALSE"]
##nn with learning 0.003 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.003, hidden=c(10,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.003nodes10<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.003nodes10)
table(test.nn$Outcome == test.nn$rate0.003nodes10)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.003nodes10)["FALSE"]
#--------------------------------learning 0.01------------------------------------------------
##nn with learning 0.001 hidden 2 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(2,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes2<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes2)
table(test.nn$Outcome == test.nn$rate0.001nodes2)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes2)["FALSE"]
##nn with learning 0.01 hidden 3 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(3,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes3<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes3)
table(test.nn$Outcome == test.nn$rate0.001nodes3)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes3)["FALSE"]
##nn with learning 0.001 hidden 4 *****not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(4,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes4<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes4)
table(test.nn$Outcome == test.nn$rate0.001nodes4)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes4)["FALSE"]
##nn with learning 0.001 hidden 5 ******not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(5,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes5<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes5)
table(test.nn$Outcome == test.nn$rate0.001nodes5)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes5)["FALSE"]
##nn with learning 0.001 hidden 6 ***** not converged
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(6,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes6<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes6)
table(test.nn$Outcome == test.nn$rate0.001nodes6)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes6)["FALSE"]
##nn with learning 0.001 hidden 7
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(7,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes7<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes7)
table(test.nn$Outcome == test.nn$rate0.001nodes7)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes7)["FALSE"]
##nn with learning 0.001 hidden 8
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(8,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes8<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes8)
table(test.nn$Outcome == test.nn$rate0.001nodes8)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes8)["FALSE"]
##nn with learning 0.001 hidden 9
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(9,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes9<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes9)
table(test.nn$Outcome == test.nn$rate0.001nodes9)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes9)["FALSE"]
##nn with learning 0.001 hidden 10
nn <- neuralnet(f, data=train.nn, linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.001, hidden=c(10,2))
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$rate0.001nodes10<-ifelse(predict.nn$net.result>0.5,1,0)
nn
table(test.nn$Outcome, test.nn$rate0.001nodes10)
table(test.nn$Outcome == test.nn$rate0.001nodes10)["TRUE"]
table(test.nn$Outcome == test.nn$rate0.001nodes10)["FALSE"]


