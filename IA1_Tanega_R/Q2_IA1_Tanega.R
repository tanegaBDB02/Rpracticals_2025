fibonacci<- function(n){
  a=0 #initiliasing the numbers 
  b=1
  print(a)
  print(b)
  
  s<- seq(2,n) #taking a sequence from 2 to n
  for (i in s){
    fibo=a+b
    a=b #swapping the numbers
    b=fibo #swap
    print(fibo)
  }
}
  
print(fibonacci(25))
