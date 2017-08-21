#1
hw2q1 = function(x){
  if(0 <= x%%360 && x%%360 < 90)
  {
    print("Quadrant 1")
  }else if(90 <= x%%360 && x%%360 < 180)
  {
    print("Quadrant 2")
  }else if(180 <= x%%360 && x%%360 < 270)
  {
    print("Quadrant 3")
  }else
  {
    print("Quadrant 4")
  }
}


#2
hw2q2 = function(x){
  x.min = x[1]
  for (i in 1:length(x))
  {
    if(x[i] < x.min)
    {
      x.min = x[i]
    }
  }
  show(x.min)
}


#3
hw2q3 = function(){
  x = sample(1:6, 1)
  y = sample(1:6, 1)
  if(x+y == 7 || x+y == 11)
  {
    print("You win!")
  }else if(x+y == 2 || x+y == 3 || x+y == 12)
  {
    print("You lose!")
  }else
  {
    point = x+y
    x = 0; y = 0;
    while(x+y != point && x+y != 7)
    {
      x = sample(1:6, 1)
      y = sample(1:6, 1)
    }
    if(x+y == point)
    {
      print("You win!")
    }else
    {
      print("You lose!")
    }
  }
}


#4
x = rep(100, 10)
random.sum = function(n){
  x[1:n] = ceiling(10*runif(n))
  cat("x:", x[1:n], "\n")
  return(sum(x))
}
#x is initialized to be 10 length of 100's
#and only the first n numbers are replaced.
#changing x to be initialized at 0 would fix
#this issue, but the function still would only
#work up to n = 10.

#This function works as intended for any whole number n
random.sum = function(n){
  x = rep(0, n)
  x[1:n] = ceiling(10*runif(n))
  cat("x:", x[1:n], "\n")
  return(sum(x))
}