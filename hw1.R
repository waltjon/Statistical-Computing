#1
#moves the hundredths place to the ones place and removes decimals
x = floor(pi*100)
#returns the last integer which was originally the hundredths place
as.double(substr(as.character(x), start=nchar(x), stop=nchar(x)))
#outputs 4


#2
#initializes an empty vector 
emptyvec = c()
#adds i, i times to empty vector
for (i in 1:5)
{
  emptyvec = c(emptyvec, rep(i, i))
}
emptyvec
#outputs [1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5] 

#initializes rows and binds them together
row1 = c(0, 2, 3)
row2 = c(0, 5, 0)
row3 = c(7, 0, 0)
rbind(row1, row2, row3)


#3
#initializes f(x) function
f = function(x) ((1/sqrt(2*pi))*(exp(1)^-((x^2)/2)))
#integrates function on the interval 1 to 2
integrate(f, lower = 1, upper = 2)
#outputs 0.1359051 


#4
#initializes matrices A and B
A = matrix(c(2, 7, 12, 3, 6, 4, 12, 15, 7), nrow = 3, ncol = 3, byrow = T)
B = matrix(c(1, 8, 2, 3, 12, 7, 9, 5, 6), nrow = 3, ncol = 3, byrow = T)
#element by element product
A*B
#matrix product
A%*%B


#5
#initializes vectorx
vectorx = c(9, -3, 2, 5)
#runs for loop which gives a vector of length vectorx
#each element is (xi-xbar)^2
standard = c();
for(i in 1:length(vectorx))
{
  standard = c(standard, (vectorx[i]-mean(vectorx))^2)
}
#finds the standard deviation
sqrt(sum(standard)/(length(vectorx)-1))
#outputs 5.057997
sd(vectorx)
#outputs the same, 5.057997