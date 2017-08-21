#1
#install.packages('mlbench')
library(mlbench)
data(BreastCancer)

list = rowSums(is.na(BreastCancer))
Data = BreastCancer[list==0,]

n = dim(Data)[1]
set.seed(1000)
train.index = sample(1:n, size=341)
train.X = Data[train.index,2:10]
train.Y = Data[train.index,11]
test.X = Data[setdiff(1:n, train.index),2:10]
test.Y = Data[setdiff(1:n, train.index),11]

library(class)
mod2 <- knn(train.X, test.X, train.Y, k=3, prob = T)

#creates and outputs the table
table = data.frame(mod2, test.Y)
table

#Calculates and outputs the misclassification
wrong = 0
for (i in 1:dim(table)[1]) {
  if(table[i, 1] != table[i, 2]){
    wrong = wrong+1
  }
}
wrong/dim(table)[1] #0.03508772

#
train.X1 = apply(train.X, 2, as.numeric)
train.Y1 = as.numeric(train.Y=='malignant')
test.X1 = apply(test.X, 2, as.numeric)
test.Y1 = as.numeric(test.Y=='malignant')

trainset = data.frame(train.X1, train.Y1)
colnames(trainset)=c(colnames(train.X), 'train.Y')

#install.packages('neuralnet')
library(neuralnet)

# come up with the formula f.
feats = names(train.X)
f <- paste(feats,collapse=' + ')
f <- paste('train.Y ~',f)
f <- as.formula(f)
# run neural net model with no hidden layer.
nn <- neuralnet(f, data=trainset, hidden=0, linear.output=FALSE)

#outputs nn plot
plot(nn)

# Perform prediction.
pred.nn <- compute(nn, test.X1)
cbind(pred.nn$net.result, test.Y1)

# using 0.5 as the treshold. 
test.Yhat = pred.nn$net.result>0.5
cbind(test.Yhat,test.Y)
misclass.rate = mean(abs(test.Yhat-test.Y1))
misclass.rate





#2
#install.packages('bootstrap')
library(bootstrap)
data(law)

theta.hat = cor(law$LSAT, law$GPA)
theta.hat #0.7763744913

data = rnorm(200, mean=theta.hat, sd=1)

# (1) Get Bootstrap samples of theta.hat
B = 1000
theta.hat.star = rep(NA, B)
for (i in 1:B){
  resample.i= sample(data, size=length(data), replace=TRUE)
  theta.hat.star[i] = mean(resample.i)
}

# (2) Plot the bootstrap distribution
# the histogram of bootstrap distribution.
hist(theta.hat.star,breaks=20, freq=FALSE)

# (3) Now calculate the bootstrap estimation of mean, sd, and 95% CI of X.bar
mean1 = mean(theta.hat.star)
sd1 = sd(theta.hat.star)
CI951 = quantile(theta.hat.star, prob=c(0.025, 0.975))

mean1
sd1
CI951 #[0.5934573028, 0.8924018714]
