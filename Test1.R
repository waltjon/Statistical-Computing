#15
abs.shit = function(x){
  if(x<0){
    x=-x
  }
  return(x-1)
}



#16
cdf = function(x){
  if(x <= 2){
    return(0)
  }else if(2<x && x<=3){
    return(x-2)
  }else{
    return(1)
  }
}
#16.1
random.samples = sample(10, 1000, replace = T)
cdf.samples = c()
for (i in 1:length(random.samples)) {
  cdf.samples = c(cdf.samples, cdf(random.samples[i]))
}
cdf.samples
#16.2
hist(cdf.samples)



#17
xgrid = seq(1,3,length.out = 100)
win.graph()
x11()
plot(xgrid, (sqrt(xgrid)+1)^(1/4), type='l', main='g(x)=(sqrt(x)+1)^(1/4)', cex.main=2, cex.lab=2)
abline(v=3, col='red',lty=2)

# step 1: generate unif(1,3) samples.
n.s = 1000
x.sample = runif(n.s, min=1, max=3) 

# step 2:  calculate g(x), 
g.x = (sqrt(x.sample)+1)^(1/4)

# step 3: approxiate I(g)
I.est = (3-1)*mean(g.x)
I.est
#I.est is the answer 2.486746