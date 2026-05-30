double_factorial <- function(n) 
{
  prod=1 #initialising prod variable to 1 for multiplication
  x=(n+1)/2
  y=seq(1,x,2) #sequence of numbers till n with steps of 2
    
  for(i in y){
    prod=prod*((2*i)-1) #applying formula
    }
  return(prod)
}

print(double_factorial(11)) 


double_factorial_even<- function(n)
{
  prod=1
  x=n/2
  s=seq(2,x,2)
  
  for (i in s){
      prod=prod*(2*i) #applying formula
  }
  return(prod)
}

print(double_factorial_even(12))
