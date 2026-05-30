### EXERCISES
#Q1
x=seq(10,120,10)
amat=matrix(x,byrow=TRUE,nrow=3)
print(amat)
amat2=matrix(x,byrow=FALSE,nrow=3)
print(amat2)
#Difference between amat and amat2 is that with byrow=FALSE the elements are distributed column-wise, first column is filled, then second column is filled and so on.

print(rownames(amat))
rownames(amat) <- rownames(amat,do.NULL=FALSE,prefix="R")
colnames(amat) <- c("C1","C2","C3","C4")
print(colnames(amat))
print(rownames(amat))
print(amat)

x<- c(2,5,7,3,1,8,9,10,1,12,5,10,4,17,15,11)
A=matrix(x,nrow=4) #byrow=FALSE is default
print(A)
y<- c(12,5,3,17,1,18,9,10,1,12,5,10,4,15,15,4)
B=matrix(y,nrow=4)
print(B)
print(A %*% B) #element wise multiplication
print(A*B) #matrix-matrix multiplication

X <- c(5,6,8,9)
Y <- c(8,10,12,5)
print( X*Y) #element wise multiplication
print( X %*% Y)
print( X %o% Y)
print(diag(X))
print(diag(6))

A <- matrix(c(3,4,-2,4,-5,1,10,-6,5),nrow=3)
print(A)
B <- matrix(c(5,-3,13),nrow=3)
print(B)
X=solve(A,B)
print(X)
print(class(X))
print(typeof(X))

Ainv=solve(A)
print(Ainv)
print(round(Ainv %*% A,1))

results=eigen(A)
print(results)
print(typeof(results))
print(typeof(results$values))
print(typeof(results$vectors))
print(results$values[1]*results$vectors[,1])
print(A %*% results$vectors[,1])
print('The above is same as results$values[1]*results$vectors[,1]')

### Q2
data=read.csv("D:/ACADEMIC/TEACHING/TEACHING_IN_IBAB/FY2024-2025/BDBP209/Labs/DATA/BrainCancer.csv",header=TRUE)
data$newcol <- data$gtv^2 + data$time 
print(names(data))
print(rownames(data))
rownames(data) <- c(paste("Row-",1:dim(data)[1],sep=""))
print(rownames(data))
data$ki <- NULL
print(names(data))

### Q3
#install.packages(readxl)
library(readxl)
data <- read_excel("D:/ACADEMIC/TEACHING/TEACHING_IN_IBAB/FY2024-2025/BDBP209/Labs/DATA/pone.0148733.s001.xlsx",1)
print(dim(data))
print(names(data))

### Q4
setA <- c("a","b","c","d","e")
setB <- c("d","e","f","g")
print(setA)
print(setB)
print(union(setA,setB))
print(intersect(setA,setB))
print(setdiff(setA,setB))
print(setdiff(setB,setA))
new_union <- c(setdiff(setA,setB),intersect(setA,setB),setdiff(setB,setA))
print(setequal(new_union,union(setA,setB)))
print("Listing elements of B in A: 3 ways")
print(intersect(setA,setB))
print(setdiff(setA,setdiff(setA,setB)))
print(setA[setA %in% setB])

### Q5
vec <- c(8,10,12,7,14,16,2,4,9,19,20,3,6)
print(subset(vec,vec>12))
print(subset(vec,vec>10 & vec<20))
A <- as.array(c(2,7,29,32,41,11,15,NA,NA,55,32,NA,42,109))
B <- A[!is.na(A) & A<100]
print(B)
A[is.na(A)] <- 0
print(A)

# creating a vector of gene names
genes = c("gene-1","gene-2","gene-3","gene-4","gene-5","gene-5","gene-6")
# creating a vector of gender 
gender = c("M", "M", "F", "M", "F", "F", "M")
# creating 7 data vectors with experimental results
result1 = c(12.3, 11.5, 13.6, 15.4, 9.4, 8.1, 10.0)
result2 = c(22.1, 25.7, 32.5, 42.5, 12.6, 15.5, 17.6)
result3 = c(15.5, 13.4, 11.5, 21.7, 14.5, 16.5, 12.1)
result4 = c(14.4, 16.6, 45.0, 11.0, 9.7, 10.0, 12.5)
result5 = c(12.2, 15.5, 17.4, 19.4, 10.2, 9.8, 9.0)
result6 = c(13.3, 14.5, 21.6, 17.9, 15.6, 14.4, 12.0)
result7 = c(11.0, 10.0, 12.2, 14.3, 23.3, 19.8, 13.4)
# creating a data frame with this data.
# genes along rows, results along columns
datframe = data.frame(genes,gender,result1,result2,result3,result4,
                      result5,result6,result7)
# adding column names to data frame
names(datframe) = c("GeneName", "Gender", "expt1", "expt2", "expt3", "expt4",
                    "expt51", "expt52", "expt6")
# creating subset of data with expt2 values above 20
subframe1 = subset(datframe, datframe$expt2 > 20)
# creating a subset of data with only Female gender
subframe2 = subset(datframe, datframe$Gender == "F")
# creating a subset with male gender for which expt2 is less than 30 
subframe3 = subset(datframe, (datframe$Gender == "M")&(datframe$expt2 < 30.0) )
# printing the data frames
print("subframe1 : Rows with expt2 > 20")
print(subframe1)
print("subframe2 : Rows with gender Female")
print(subframe2)
print("subframe3 : Rows with Male gender and expt2 < 30.0")
print(subframe3)

### Q6
# theta is the angle in degrees:
theta = 45
if(theta == 0) {
  print("Positive X axis")
} else if ( (theta > 0) & (theta < 90)) 
{
  print("First quadrant")
} else if (theta == 90)
{
  print("Positive Y axis")
} else if ( (theta > 90) & (theta < 180)) 
{
  print("Second quadrant")
} else if (theta == 180)
{
  print("Negative X axis")
} else if ( (theta > 180) & (theta < 270)) 
{
  print("Third quadrant")
} else if (theta == 270)
{
  print("Negative Y axis")
} else if ( (theta > 270) & (theta < 360)) 
{
  print("Fourth quadrant")
} else if (theta == 360)
{
  print("Positive X axis")
}

a=2
b=4
c=8
large=a
if (a>b){
  if (a>c){
    largest=a
    if (c>b){
      largest2=c
      smallest=b
    }else
    { largest2=b
      smallest=c}
  }else
  {largest=c
   largest2=a
   smallest=b}
}else if (a<b)
{ if (b>c){
    largest=b
    if (c>a){
      largest2=c
      smallest=a
    }else {
      largest2=a
      smallest=c
    }
} else if (b<c){
     largest=c
     largest2=b
     smallest=a
   }
}
print(paste('Decreasing order of input numbers:',largest,largest2,smallest))

dist=360 
age=68
cost=100.0 #fixed
cost_remain_dist=0.0
cost_remain_dist2=0.0
print(paste("The cost for the first 100 km is:",cost))
if (dist>100){
  remain_dist=min(1000,dist)-100
  cost_remain_dist=remain_dist*1.5
  print(paste("Cost between 100 km and 1000 km: Rs.",cost_remain_dist))
}
if (dist>1000){
  remain_dist2=dist-1000
  cost_remain_dist2=remain_dist2*2.0
  print(paste("Cost above 1000 km: Rs.",cost_remain_dist2))
}

cost=cost + cost_remain_dist + cost_remain_dist2
print(paste("The cost before applying for concession: Rs.",cost))
if (age>60){
  cost = cost - 0.25*cost
} else if (age<6){
  cost = cost - 0.5*cost
}
print(paste("The cost for this traveler after concession: Rs.",cost))

## Q7- functions
## (i) 
repl_neg_zero <- function(vec){
  #for (i in 1:length(vec)){ if (vec[i]<0) vec[i]<- 0}
  #alternate method
  vec[vec<0]<- 0
  return(vec)
}
vec = repl_neg_zero(c(-1,2,-3,4))
print(vec)

## (ii) function to approximate the factorial
fac7 <- function(n){
  summ <- 1 + (1/(12*n)) + (1/(288*n^2)) - (139/(51840*n^3)) - (571/(2488320*n^4))
  fac <- (n^n)*exp(-n)*sqrt(2*pi*n)*summ
  return(fac)
  }
print(fac7(4))

## (iii) function to sum the digits of a number
numsum <- function(n){
  n <- as.numeric(n)
  input_n <- n
  s <- 0
  repeat {
    if (n==0) break
    digit <- n %% 10
    s = s+digit
    n = n %/% 10 }
  return(s)
}
print(numsum(13))
