#Sources:
#http://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
library(neuralnet)
set.seed(500)

#feature selection
numeric.data.nn <- data[,c("Age", "Years.at.address",           
                      "Current.debt", "Income", "CCJs", "Loan.amount")]
factor.data.nn <- data[,c("Gender", "Employment.status", "Country",           
                          "Postcode.area", "Own.home" , "Outcome")]
#feature scaling
maxs <- apply(numeric.data.nn, 2, max) 
mins <- apply(numeric.data.nn, 2, min)
train.nn.scaled <- as.data.frame(scale(numeric.data.nn, center = TRUE, scale = TRUE))
train.nn <- data.frame(numeric.data.nn, factor.data.nn)
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train.nn <- train.nn[index,]
test.nn <- train.nn[-index,]
#run nn
n <- names(train.nn)
f <- as.formula(paste("Outcome ~", paste(n[!n %in% "Outcome"], collapse = " + ")))
nn <- neuralnet(f, data=train.nn, hidden=c(5,3), linear.output=T)
