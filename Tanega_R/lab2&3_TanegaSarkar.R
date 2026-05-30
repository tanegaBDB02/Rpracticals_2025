#Lab 2 and 3 

#Q1.1
x <- round(12.1343,digits=3)
print(x)

#Q1.2
x <- round(123.12344,digits=3)
print(x)

#Q1.3
x <- round(1234.12344,digits=3)
print(x)

#Q1.4
x <- round(12345.12344,digits=3)
print(x)

#Q1.5
options(digits=15)
x <- round(1234.12344,digits=3)
print(x)
y <- round(12345.12344,digits=3)
print(y)

#Q1.6
x <- formatC(round(12345.12344,digits=3),format="f",digits=3)
print(x)

#Q1.7
x <- (1234.12344)
print(x)

#Q1.8
print (1234.723, digits=3)
print (1234.723,digits=5)

#Q1.9
x <- round(123456788.123,digits=3)
print(x)

#Q1.10
print (round(123456788.123,digits=2),digits=20)

#Q1.11
print (round(123456789.1234,digits=4),digits=20)

#Q1.12
x <- paste("Hello World")
print(x)
y<- paste("Hello","World")
print(y)

#Q1.13
x <- paste(1:10)
print(x)
y<- paste(1:10)[4]
print(y)

#Q1.14
x<- as.numeric(paste(1:10))
print(x)

#Q1.15
x<- paste(1:10,collapose=".")
print(x)

#Q1.16
x<- paste(c("Hello","World"),1:10,sep="-")
print(x)
y<- print(paste("Hello",1:10,sep="-"))
print(y)



#Q2.1
x<-0:10
print(x)

#Q2.2
x<- 15:5
print(x)

#Q2.3
x<- seq(0,1.5,0.1)
print(x)

#Q2.4
x<- seq(6,4,-0.2)
print(x)

#Q2.5
N <- c(55,76,92,103,84,88,121,91,65,77,99)
print(N)

#Q2.6
x<-seq(from=0.04,by=0.01,length=11)
print(x)
y<-seq(0.04,by=0.01,along=N)
print(y)

#Q2.7
x<-seq(from=0.04,to=0.14,along=N)
print(x)

#Q2.8
x<-sequence(c(4,3,4,4,4,5))
print(x)

#@2.9
x<- rep(9,5)
print(x)
y<- rep(1:4,2)
print(y)
z<-rep(1:4,each=2)
print(z)
a<- rep(1:4,each=2,times=3)
print(a)
b<- rep(1:4,1:4)
print(b)

#2.10
x<- rep(1:4,c(4,1,4,2))
print(x)
y<- rep(c("cat","dog","goldfish","rat"),c(2,3,2,1))
print(y)

#Q2.11
x<- seq(-1,1,by=0.1)
print(x)
y<- seq(-1,1,0.1)
print(y) 

#Q2.12
x<- seq(-1,1,length=7)
print(x)

#Q2.13
numbers <- -1 + 0.1 * (0:20)
print(numbers)




#3.1
x<- 3/0
print(x)

#3.2
x<- exp(-Inf)
print(x)

#3.3
x<- (0:3)**Inf
print(x)

#3.4
x<- 0/0
print(x)

#3.5
x<- Inf - Inf
print(x)

#3.6
x<- Inf/Inf
print(x)

#3.7
x<- is.finite(10)
print(x)

#3.8
x<- is.infinite(10)
print(x)

#3.9
x<- is.infinite(Inf)
print(x)

#3.10
y<- c(4,NA,7)
y=="NA"
is.na(y)

#3.11
y[!is.na(y)]

#3.12
c1<- c(1,2,3,NA)
c2<- c(5,6,NA,8)
c3<- c(9,NA,11,12)
c4<- c(NA,14,15,16)
full.frame <- data.frame(c1,c2,c3,c4)
reduced.frame <- full.frame[! is.na(full.frame$c1),]
print(full.frame)
print(reduced.frame)

#3.13
v <- c(1:6,NA,NA,9:12)
seq(along=v)[is.na(v)]
which(is.na(v))



#Q4.1
vec <- c(4,7,6,5,6,7)
class(vec)
length(vec)
min(vec)
max(vec)

#4.2
vec <- scan()

#4.3
vec[4]

#4.4
ind <- c(2,3,6)
vec[ind]
vec[c(2,3,6)] #alternate way

#4.5
vec[-1]

#4.6
vec[-length(vec)]

#4.7
trim <- function(x) sort(x)
sorted_vec <- trim(vec)
smallest_two <- sorted_vec[1:2]
largest_two <- sorted_vec[(length(sorted_vec) - 1):length(sorted_vec)]
print(smallest_two)
print(largest_two) #not a one line function

#4.8
vec[1:3]
vec[seq(2,length(vec),2)]
#to get the elements that have even indices
vec<- c(2,4,3,8,1,5)
even_index_elements <- vec[(1:length(vec)) %% 2 == 0]
print(even_index_elements)

#4.9
x <- 0:10
sum(x[x<5])

#4.10
#sum of three largest values of a vector
vec<- c(3,7,2,9,6)
sorted_vec<- sort(vec)
sorted_vec[(length(sorted_vec)-2):length(sorted_vec)]
sum_large_three_nums<- sum(sorted_vec)
print (sum_large_three_nums)

#4.11
vec<- c(3,7,2,9,6)
which.max(vec) #gives the index and not the element
which.min(vec)

#4.12
cbind(1:10,10:1)
rbind(1:10,10:1)

#4.13
X <- c(1:10)
X
Y <- c(1:10*5)
Y
X*Y
X+Y
X/Y
X^Y
log(X)
exp(Y)



#5.1
y <- 1:24
dim(y) <- c(2,4,3)
y

#5.2
X <- matrix (c(1,0,0,0,1,0,0,0,1),nrow=3)
print(X)

#5.3
vector <- c(1,2,3,4,4,3,2,1)
V <- matrix(vector,byrow=T,nrow=2) #method 1
V
#to print column wise
#V <- matrix(vector,byrow=F,nrow=2) #method 1
#V
dim(vector) <- c(4,2) #method 2
is.matrix(vector)
vector
dim(vector) <- c(4,2,1) 
is.matrix(vector)
vector


#6.1
vec<- c(2,4,6,3,8)
print (min(vec))
print (max(vec))
print(sum(x))
print(range(x))
print(sort(x))

#6.2
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
colMeans(M)  

#6.3
Z <- X[1:4] %o% Y[1:3]
Z
YoX <- Y[1:3] %o% X[1:4]
YoX
t(Z) #transpose
t(YoX)
X <- matrix(c(1, 2, 3), nrow = 1)  
Y <- matrix(c(4, 5, 6), ncol = 1) 
dot<- X %*% Y #dot product
print(dot)
X <- matrix(c(1, 2, 3), nrow = 1)  
Y <- matrix(c(4, 5, 6), nrow = 1) 
sum(X*Y) #another way to carry out dot product
crossprod(X[1:4],Z)
diag(4) #identity matrix

#6.4
class(X)