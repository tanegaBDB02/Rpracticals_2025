#1
operations<- function (a,b){
  sum<- a+b
  diff<- a-b
  prod<- a*b
  div<- a/b
  print("The sum, difference, product and quotient of the two numbers are respectively")
  print(sum)
  print(diff)
  print(prod)
  print(div)
}
operations(2,3)
  
#2
quad<- function(a,b,c){
  pos_rt<- (((-b)+(((b*b)-(4*a*c))*0.5))/(2*a))
  print(pos_rt)
  neg_rt<- (((-b)-(((b*b)-(4*a*c))*0.5))/(2*a))
  print(neg_rt)       
}
quad(4,5,6)


