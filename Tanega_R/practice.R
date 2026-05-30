#deleting a column from a dataframe

data$time<- NULL
print (colnames(data))



a=5.0
b=10.0
if (a<b)
  print("a is less than b")

a=5.0
b=10.0
c=15.0
d=20.0
if (a>b)
{
  print("a is greater than b")
}
if (a>b)
{
  print("a>b")
} else if (b<c)
{
  print("b<c")
}

for (i in 1:5) print (i^2)
j=0
k=1
for (i in 1:5){
  j=j+1
  k=k+2
  print(paste(j,k))
}

fn1<- function(x) x^2
print(fn1(3))

fac1<- function(x){
  f<- 1
  if (x<2) return (1)
  for (i in 2:x){
    f<- f*i
    f}
  return(f)
}

print(fac1(4))
sapply(0:5,fac1)

fac2 <- function(x) {
  f <- 1
  t <- x
  while (t > 1) {
    f <- f * t
    t <- t - 1
  }
  return(f)  
}

print(fac2(5))

fac3<- function (x){
  f<-1
  t<-x
  repeat{
    if (t<2) break
    f<- f*t
    t<- t-1}
  return(f)
}

print(fac3(5))


fac4<- function(x) max(cumprod(1:4))
fac4(5)

pc<- proc.time()
result1 <- fac1(100000)
print(proc.time()- pc)

x<- runif(1000000000)
pc<- proc.time()
cmax<- x[1]
for (i in 1000000000){
  if (x[i] < cmax) cmax<- x[i]}
proc.time()-pc

