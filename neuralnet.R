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

#run nn
n <- names(train.nn)
f <- as.formula(paste("Outcome ~", paste(n[!n %in% "Outcome"], collapse = " + ")))
nn <- neuralnet(f, data=train.nn, hidden=c(5), linear.output=F, stepmax= 1e+05, threshold=0.01, algorithm='backprop', learningrate=0.01)
#plot nns
plot.nn(nn)
#predict nn
predict.nn <- compute(nn,test.nn[,1:11])
test.nn$predict <- ifelse(predict.nn$net.result>0.5,"paid","defaulted")
#confusion matrix
table(test.nn$predict, test.nn$Outcome)

