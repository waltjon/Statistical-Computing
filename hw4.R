#1
x.sample2 = rep(NA, 500)
for (i in 1:length(x.sample2)){
  u.i = runif(1, min=0, max=1) 
  x.i = 0
  while (ppois(x.i, 2)< u.i){
    x.i = x.i + 1
  }
  x.sample2[i] = x.i
}
par(mfrow=c(1,2), pty='s')
hist(x.sample2)
hist(rpois(500, 2))


#2
win.graph()
x11()
par(mfrow=c(1,2), pty='s')
x =seq(1,2,length.out=1000)
plot(x, 3*(x-1)^2, ylim=c(0,3.5), type="l", main='pdf')
abline(h=3, lty=2)

n.s=1000
x.all=cbind(runif(n.s, min=1, max=2), runif(n.s, min=0,max=3))
points(x.all[,1], x.all[,2], pch=1, cex=0.5)

flag = rep(0, n.s)
for (i in 1:n.s){
  x.i = x.all[i, ]
  f.x = 3*(x.i[1]-1)^2
  if (x.i[2]<f.x){
    flag[i]=1     
  }  
}

x.sample = x.all[flag==1,1]
points(x.all[as.logical(flag),1], x.all[as.logical(flag),2], col="red", pch=2, cex=0.5)
acceptance.rate=mean(flag)

hist(x.sample, breaks=20, freq=FALSE)
lines(x, 3*(x-1)^2, type="l", col='red',lwd=2)


#3
# First, plot f(x) and c g(x).
x.range = seq(0, 2.5, length.out=100)
win.graph()
x11()
plot(x.range, (1/sqrt(2*pi))*exp(-x.range^2/2),type='l', ylim=c(0,1.5),col='red')

lines(x.range, 1.656*(1/pi)*(1/(1+x.range^2)), col='blue')

# Second, write a function for the rejection method
sample.func= function(n, c, plt){
  # This function generate gamma(2,3) random samples.
  # n: sample size.
  # c: the c so that f(x)<c g(x).
  # plt: 1 or 0, whether add a points on the plot. 
  out = rep(NA, n)
  for (i in 1:n){    
    ratio = 0
    u = 1
    while (u > ratio){ # loop until we get u<=ratio.
      
      # step 1. Generate from x~ exp(2), and u~unif(0,1)
      x.i = rexp(1, rate=2)
      u = runif(1, min=0, max=1)
      
      # step 2. accept or reject.
      f.x = (1/sqrt(2*pi))*exp(-x.i^2/2)
      g.x = (1/pi)*(1/(1+x.i^2)) # or dexp(x.i, rate=2) 
      ratio = f.x/(c*g.x)
      
      if (plt==1){
        points(x.i, 0, col=ifelse(u > ratio, 'red', 'blue'), pch=16, cex=0.5)
        Sys.sleep(0.005) 
      }
    }
    
    out[i]=x.i    
  }  
  return(out)  
}


# plot the histogram
win.graph()
x11()
plot(x.range, (1/sqrt(2*pi))*exp(-x.range^2/2),type='l', ylim=c(0,1.5),col='blue', main='The general rejection method')
lines(x.range, 1.656*(1/pi)*(1/(1+x.range^2)), col='red')
sample = sample.func(n=1000, c=1.52, plt=1)
# add histogram
hist(sample, breaks=20, freq=FALSE, add=TRUE)


#4
xgrid = seq(-1,2,length.out = 100)
win.graph()
x11()
plot(xgrid, cos(sqrt(xgrid^3+1))^2, type='l', main='g(x)=cos(sqrt(x^3+1))^2', cex.main=2, cex.lab=2)
abline(v=2, col='red',lty=2)



# step 1: generate unif(-1,2) samples.
n.s = 1000
x.sample = runif(n.s, min=-1, max=2) 

# step 2:  calculate g(x), 
g.x = cos(sqrt(x.sample^3+1))^2

# step 3: approxiate I(g)
I.est = (2+1)*mean(g.x)
I.est

win.graph()
x11()
plot(x.sample, g.x, type="h", main='the g(X_i) and mean(g(X_i))')
abline(h=mean(g.x), col="red",lwd=2)