#Labs 2 and 3
###############  Q1 ##########################
print(round(12.1343,digits=3)) #12.134
print(round(123.12344,digits=3)) #123.123
print(round(1234.12344,digits=3)) #1234.123
print(round(12345.12344,digits=3)) #12345.12
x<- round(12345.12344,digits=3)
options(digits=15)
print(x) #12345.123
options(digits=20)
print(x) #12345.122999....593 because 20 places allows R to print the numerical noise as well
print(formatC(x,format='f',digits=3),quote=FALSE) # this will match the options=15 output although currently options=20
#restart R and then do the below
#default print options, except with options=20
print(1234.12344) #we get 1234.123
print(1234.723,digits=4) #1235
print(1234.12344,digits=5) #1234.1
print(round(123456788.123,digits=3)) #123456788.12299999595
print(round(123456788.123,digits=2),digits=20) #123456788.12000000477
print(round(123456789.1234,digits=4),digits=20) #123456789.12340000272

#sep option in paste is betn 2 arguments/terms in the function, collapse is betn elements within a term
# Essentially paste is a concatenation operation between 2 objects after converting them to strings.
print(paste("Hello World"))
print(paste("Hello","World"))
print(paste(1:10),quote=FALSE)
print(paste(1:10)[4],quote=FALSE)
print(class(paste(1:10))) #character
print(class(as.numeric(paste(1:10)))) #numeric
print(paste(1:10,collapse='.')) #1.2.3.4.5.6.7.8.9.10
print(paste(c("Hello","world"),1:10,sep='-'),quote=FALSE) #Hello-1 world-2 Hello-3 and so on
print(paste("Hello",1:10,sep='-'),quote=FALSE)
print(c(paste(1:10),paste(11:20)),sep='==') #no effect of using this sep because such an option does not exist in print()
print(paste("J","N",1,2,3,1:5,sep='++',collapse='.'),quote=FALSE) #J++N++1++2++3++1.J++N++1++2++3++2.J++N++1++2++3++3.J++N++1++2++3++4.J++N++1++2++3++5

####################### Q2 #######################
options(digits=8)
print(0:10)
print(15:5)
print(seq(0,1.5,0.1))
print(seq(6,4,-0.2))
N <- c(55,76,92,103,84,88,121,91,65,77,99)
print(seq(from=0.04,by=0.01,along=N))
print(seq(from=0.04,to=0.14,along=N)) #this matches with previous
print(sequence(c(4,3,4,4,4,5))) 
# 1 2 3 4 1 2 3 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 5
print(rep(9,5)) # 9 9 9 9 9 
print(rep(1:4,2)) #1 2 3 4 1 2 3 4
print(rep(1:4,each=2)) #1 1 2 2 3 3 4 4
print(rep(1:4,each=2,times=3)) # 1 1 2 2 3 3 4 4 1 1 2 2 3 3 4 4 1 1 2 2 3 3 4 4
print(rep(1:4,1:4))# 1 2 2 3 3 3 4 4 4 4
print(rep(1:4,c(4,1,4,2)))
print(rep(c("cat","dog","goldfish","rat"),c(2,3,2,1)))
print(seq(-1,1,by=0.1))
print(seq(-1,1,0.1))# same as above
print(seq(-1,1,length=7))
#to generate numbers from -1 to 1 with an interval of 0.1 without using seq()
#The number of elements in this set is (1-(-1))/0.1=20 
nos <- 0:20
print(nos)
sequen <- -1 + (0.1)*nos
print(sequen)
print(seq(0:10)) #1 2 3 4 5 6 7 8 9 10 11
print(seq(0,10)) #0 1 2 3 4 5 6 7 8 9 10
print(0:10) #this has same o/p as seq(0,10)
print(seq(2))# generates number from 1 to specified number
print(seq(0)) #1 0
print(seq(-2)) # 1 0 -1 -2 same o/p as seq(1,-2)

########################### Q3 #######################
print(3/0)
print(exp(-Inf))
print((0:3)**Inf)
print(0/0)
print(Inf-Inf)
print(Inf/Inf)
print(is.finite(10))
print(is.infinite(10))
print(is.infinite(Inf))
y<- c(4,NA,7)
print(y=='NA') #FALSE NA FALSE
print(is.na(y)) #FALSE TRUE FALSE
print(y[!is.na(y)])
c1 <- c(1,2,3,NA)
c2 <- c(5,6,NA,8)
c3 <- c(9,NA,11,12)
c4 <- c(NA,14,15,16)
full.frame <- data.frame(c1,c2,c3,c4)
print(full.frame)
#get rid of the row containing NA that has NA in column1
reduced.frame <- full.frame[! is.na(full.frame$c1),]
print(reduced.frame)
#This will do the above and print only the first column.
print(full.frame[! is.na(full.frame$c1),1])
#This will do the above and print only the second column. This may contain NA because NA has been removed according to column 1
print(full.frame[! is.na(full.frame$c1),2])
print(full.frame[! is.na(full.frame),])
# The above print out is
#c1 c2 c3 c4
#1     1  5  9 NA
#2     2  6 NA 14
#3     3 NA 11 15
#NA   NA NA NA NA
#NA.1 NA NA NA NA
#NA.2 NA NA NA NA
#NA.3 NA NA NA NA
#NA.4 NA NA NA NA
#NA.5 NA NA NA NA
#NA.6 NA NA NA NA
#NA.7 NA NA NA NA
#NA.8 NA NA NA NA
print(full.frame[! is.na(full.frame)])
#The output is just 1 2 3 4 5 6 8 9 11 12 14 15 16
print(full.frame[! is.na(full.frame),1]) #o/p is 1 2 3 NA NA NA NA NA NA NA NA NA
print(full.frame[1,])
print(full.frame[c1,])
#The output is 
#c1 c2 c3 c4
#1   1  5  9 NA
#2   2  6 NA 14
#3   3 NA 11 15
#NA NA NA NA NA
#The last row has NA for all c's! This is because c1 is used as the row header, and 
#when it encountered NA as the 4th row, since NA is not a number, it assigned all the elements
#as NA for all the columns.
v<- c(1:6,NA,NA,9:12)
print(seq(along=v)[is.na(v)])
#The output is 7 8
print(which(is.na(v))) #o/p is 7 8

#################### Q4 #####################################
vec <- c(4,7,6,5,6,7)
print(class(vec))
print(length(vec))
print(min(vec))
print(max(vec))
#vec <- scan()
print(vec)
print(vec[4])
ind <- c(2,3,6)
print(vec[ind])
print(vec[-1]) #removes first element
print(vec[-length(vec)]) #removes last element
trim <- function(x) sort(x) [-c(1,2,length(x)-1,length(x))] #function to remove two largest and two smallest values from a vector
print(trim(vec))
print(vec[1:3])
print(vec[seq(2,length(vec),2)])
print(vec[1:length(vec)%%2==0]) #alternate way to do the above
x <- 0:10
print(x)
print(sum(x[x<5]))
print(sum(x[length(sort(x))-2],x[length(sort(x))-1],x[length(sort(x))])) #method 1
sum_lar3 <- function(x){ 
  y <- sort(x)
  result <- sum(y[length(y)-2],y[length(y)-1],y[length(y)])
  return(result)
}
print(sum_lar3(x)) #method 2
print(which.max(x), which.min(x))
print(cbind(1:10,10:1)) # 2 columns and 10 rows are created
print(rbind(1:10,10:1)) # 10 columns and 2 rows are created
X <- c(1:10)
Y <- c(1:10*5)
print(paste(X,Y),quote=FALSE)
print(X+Y)
print(X-Y)
print(X*Y)
print(X/Y)
print(log(X))
print(exp(Y))

######################### Q5 ##################################
y <- 1:24
dim(y) <- c(2,4,3)
print(y)
X <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)
print(X)
vector <- c(1,2,3,4,4,3,2,1)
V <- matrix(vector,byrow=T,nrow=2)# method 1
print(V)
print(matrix(vector,nrow=2)) #method 2, automatically makes byrow=T
print(matrix(vector,byrow=F)) #makes 1 column of numbers
print(matrix(vector,byrow=T)) #without nrow this will also create 1 column of vector
dim(vector) <- c(4,2) #method 3
print(vector)
print(is.matrix(vector))

###################### Q6 ###########################
print(min(vec))
print(max(vec))
print(sum(vec))
print(range(vec))
print(sort(vec))
print(colMeans(vector))
Z <- X[1:4]%*%Y[1:4] #dot product
print(Z)
Z <- X[1:4]%o%Y[1:3] #outer product
print(Z)
YoX <- Y[1:3]%o% X[1:4] 
print(YoX) #relationship between YoX and Z is they are transpose of each other
print(t(Z))
print(sum(X[1:4]*Y[1:4]))
print(crossprod(X[1:4],Z)) #result is 1x3 matrix, equivalent to t(X) %*% Y
print(diag(4))
print(class(X))
print(attributes(X))