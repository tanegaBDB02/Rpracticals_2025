
#1

# Form a matrix amat of dimensions 3×4 containing numbers from 10 to 120 in
# steps of 10. Print this matrix. By default the byrow option in matrix() function
# is TRUE, so repeat the above with FALSE and form another matrix called amat2.
# Print this matrix. What is the relation between the 2 matrices?

amat <- matrix(seq(10, 120, by = 10), nrow = 3, ncol = 4, byrow = TRUE)
print("Matrix amat (byrow = TRUE):")
print(amat)
amat2 <- matrix(seq(10, 120, by = 10), nrow = 3, ncol = 4, byrow = FALSE)
print("Matrix amat2 (byrow = FALSE):")
print(amat2)
# amat fills numbers row-wise (left to right) amat2 fills numbers column-wise (top to bottom) amat2 is the transpose of amat


# Assign the row names to amat as R1, R2 and R3 and column names as C1, C2, C3, C4.
# Print the matrix again to see that the assignments have been made.
amat <- matrix(seq(10, 120, by = 10), nrow = 3, ncol = 4, byrow = TRUE)
rownames(amat) <- c("R1", "R2", "R3")
colnames(amat) <- c("C1", "C2", "C3", "C4")
print("Matrix amat with row and column names:")
print(amat)


# Form a 4x4 matrix A with the following elements: 2,5,7,3,1,8,9,10,1,12,5,10,4,17,15,11
# and another matrix B of dimensions 4x4 with the elements:
#    12,5,3,17,1,18,9,10,1,12,5,10,4,15,15,4. Print both the matrices. Perform an element -wise multiplication of the two matrices and print the result. Perform a
# matrix-matrix multiplication of the 2 matrices and print the result.
A <- matrix(c(2, 5, 7, 3, 1, 8, 9, 10, 1, 12, 5, 10, 4, 17, 15, 11), nrow = 4, ncol = 4, byrow = TRUE)
B <- matrix(c(12, 5, 3, 17, 1, 18, 9, 10, 1, 12, 5, 10, 4, 15, 15, 4), nrow = 4, ncol = 4, byrow = TRUE)
print("Matrix A:")
print(A)
print("Matrix B:")
print(B)
element_wise<- A * B
print("Element-wise multiplication (A * B):")
print(element_wise)
matrix_multiplication_result <- A %*% B
print("Matrix-matrix multiplication (A %*% B):")
print(matrix_multiplication_result)

# 
# Define a vector X with elements 5,6,8,9 and vector Y with elements 8,10,12,5.
# Obtain the outer product of the two vectors and print the result. Also obtain the
# inner product of the two vectors and print the result.
X <- c(5, 6, 8, 9)
Y <- c(8, 10, 12, 5)
outer_product <- X %o% Y
print("Outer Product of X and Y:")
print(outer_product)
inner_product <- X %*% Y
print("Inner Product of X and Y:")
print(inner_product)


#Form a diagonal matrix using the entries of the vector X above.

X <- c(5, 6, 8, 9)
n <- length(X)
diag_matrix <- matrix(0, n, n)
for (i in 1:n) {
  diag_matrix[i, i] <- X[i]
}
print("Diagonal Matrix:")
print(diag_matrix)

#trying ut with the diag function
# X <- c(5, 6, 8, 9)
# diag_matrix <- diag(X)
# print("Diagonal Matrix:")
# print(diag_matrix)

#Print the diagonal elements of A
A <- matrix(c(2, 5, 7, 3, 1, 8, 9, 10, 1, 12, 5, 10, 4, 17, 15, 11), nrow = 4, ncol = 4, byrow = TRUE)
print(A)
print(diag(A))


#Create an identity matrix of dimensions 6x6 in one line
print(diag(6))

#sharmishtha
# (v)
X <- c(5, 6, 8, 9)
diag_matrix <- diag(X)
print("Diagonal Matrix:")
print(diag_matrix)

# (vi)
A <- matrix(c(2,5,7,3, 1,8,9,10, 1,12,5,10, 4,17,15,11), nrow=4, ncol=4, byrow=TRUE)
diagonal_A <- diag(A)
print("Diagonal Elements of A:")
print(diagonal_A)

# (vii)
identity_matrix <- diag(6)
print("6x6 Identity Matrix:")
print(identity_matrix)

#tried multiple liner code
# X <- c(1,1,1,1,1,1)
# n <- length(X)
# id_matrix <- matrix(0, n, n)
# for (i in 1:n) {
#   id_matrix[i, i] <- X[i]
# }
# print("Identity Matrix:")
# print(id_matrix)

# Create a 3x3 matrix A using the elements 3,4,-2,4,-5,1,10,-6,5 with default options.
# Print A.

# Create a 3x3 matrix A with default options (column-wise filling)
A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow = 3, ncol = 3)
print(A)


#Create a 3x1 matrix B with elements 5,-3,13. Print B.
B <- matrix(c(5,-3,13), nrow = 3, ncol = 1)
print(B)

#Find the unknown vector X of the equation AX=B using the command X=solve(A,B).
#Print X to see the results. What type of object is X?
A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow = 3, ncol = 3)
B <- c(5, -3, 2)
X <- solve(A,B)# solve(A, B) computes X=A ^−1*B, solving for the unknown vector 𝑋
print(X)
print(class(X))
print(typeof(X))


# Find the inverse of matrix A by using the command Ainv = solve(A). Print the
# inverse. Check that this is indeed the inverse by calculating the matrix product
# of Ainv and A matrix and print the result. Are you getting an identity matrix?
Ainv = solve(A)
print(Ainv)
print(round(Ainv %*% A)) #rounding off the values

#hence we are getting an identity matrix
# 
# Find the eigenfunctions and eigenvalues of A by using the command eigen(A).
# Save this to an object called results. What type of object is results? Perform
# a matrix-vector multiplication operation of A and one of the eigenvectors (say the second eigenvector). What is the result? Do you understand this result? Explain.

result=eigen(A)
print(result)
# print(result$values)
# print(result$vectors)
second <- result$vectors[,2]
multiplication_result <- A %*% second
print(multiplication_result)


#2

# Create a new column of data that calculates the following: In a given row, the
# entry from ’GTV’ column should be squared and added to the ’time’ column entry
# in that row. Add this column to the existing data frame after giving it a column
# name.
data=read.csv("/home/ibab/R/BrainCancer.csv",header=TRUE)
result=data$gtv
square=result^2
data$new_col=data$time +square
print(head(data))

#Print the row and column names of the modified data.
print(rownames(data))
print(colnames(data))

#sharmistha
brain_cancer_data=read.csv("/home/ibab/R/Lab5/BrainCancer.csv",header=TRUE)
print(colnames(brain_cancer_data))
# Print first few rows to verify
print(head(brain_cancer_data))

# (i) Create a new column: (GTV^2 + time)
brain_cancer_data$new_column <- (brain_cancer_data$gtv^2) + brain_cancer_data$time

# (ii) Print row and column names
print("Row Names:")
print(rownames(brain_cancer_data))
print("Column Names:")
print(colnames(brain_cancer_data))

# Change the row names of the data such that each row has ’Row-’ followed by the
# row number. Use the paste function for this.
rownames(data) <- paste("Row", 1:nrow(data), sep="-")
print(data)


# Remove the column ’ki’ by assigning NULL value to this column. Print the modified
# data to make sure that the ‘ki’ column has indeed been removed.
data$ki <- NULL
print(colnames(data))


#3

# Install the package called readxl by using the command install.packages("readxl")
install.packages("readxl")

# Load the package in the current R environment by using the command library(readxl).
library(readxl) #loading the library

# If you have done the above correctly, your next command will load successfully to create a dataframe of the data present.
# data <- read_excel(’<path_to_pone.0148733.s001.xlsx>’,1). The 1 stands for the sheet number in the excel file.
data2=read_excel("/home/ibab/R/pone.0148733.s001.xlsx",1)

# Print the column names and dimensions of the data in the data data frame.
print(names(data2))


#4

#Define a vector A with elements a,b,c,d,e. Also define another vector B with elements d,e,f,g. Print the vectors.

setA<- c("a","b","c","d","e")
setB<- c("d","e","f","g")
print(A)
print(B)

#Perform a union operation between the two sets and print the result.
union (setA, setB)

#Perform an intersection operation between the two sets and print the result.
intersect(setA, setB)

#Perform a difference operation between the two sets and print the result.
setdiff(setA,setB)
setdiff(setB,setA)

# The function setequal() checks for the equality of two objects. Create an object
# with a sequence of A-B, A∩ B and B-A and use this function to check its contents
# with the result of the union operation above. Print the result.
setequal(c(setdiff(setA,setB),intersect(setA,setB),setdiff(setB,setA)),union(setA,setB))

#List the elements of B present in A using two different approaches.
setB[setB %in% setA]

#Print the elements of A present in B.
setA[setA %in% setB]


#5
#Form a vector with elements 8,10,12,7,14,16,2,4,9,19,20,3,6. Print the following
# filtered data from this vector: (a) values greater than 12, (b) values greater than
# 10 and less than 20
vec <- c(8,10,12,7,14,16,2,4,9,19,20,3,6)
vec[vec > 12]         
vec[vec > 10 & vec < 20]  

# Form an array A with the elements 2,7,29,32,41,11,15,NA,NA,55,32,NA,42,109.
# Form a new array from this one where the values ’NA’ have been removed and
# the values are less than 100. Print this array.

A <- c(2,7,29,32,41,11,15,NA,NA,55,32,NA,42,109)
A_filtered <- A[!is.na(A) & A < 100]
print(A_filtered)

#Assign the value 0 to the elements ’NA’ in A and print the new array.
A[is.na(A)] <- 0
print(A)        

#Create a vector with gene names “gene-1”,“gene-2” . . . “gene-6”. Create a vector for gender with entries M,M,F,M,F,F,M.

genes <- c("gene-1", "gene-2", "gene-3", "gene-4", "gene-5", "gene-6", "gene-7")
gender <- c("M", "M", "F", "M", "F", "F", "M")

#entering the following values
result1 <- c(12.3, 11.5, 13.6, 15.4, 9.4, 8.1, 10.0)
result2 <- c(22.1, 25.7, 32.5, 42.5, 12.6, 15.5, 17.6)
result3 <- c(15.5, 13.4, 11.5, 21.7, 14.5, 16.5, 12.1)
result4 <- c(14.4, 16.6, 45.0, 11.0, 9.7, 10.0, 12.5)
result5 <- c(12.2, 15.5, 17.4, 19.4, 10.2, 9.8, 9.0)
result6 <- c(13.3, 14.5, 21.6, 17.9, 15.6, 14.4, 12.0)
result7 <- c(11.0, 10.0, 12.2, 14.3, 23.3, 19.8, 13.4)

# Create a dataframe with the following columns: genes, gender, result1, result2,
# result3, result4, result5, result6, result7. Call this data frame as datframe.
dataframe1 <- data.frame(genes, gender, result1, result2, result3, result4, result5, result6, result7)
print(dataframe1)

#Add column names to this dataframe: “GeneName”, “Gender”, “expt1”, “expt2”, “expt3”, “expt4”, “expt5”, “expt6”, “expt7”.
colnames(dataframe1) <- c("GeneName", "Gender", "expt1", "expt2", "expt3", "expt4", "expt5", "expt6", "expt7")

# Create a subset of data with “expt2” values greater than 20
subset_expt2 <- dataframe1[dataframe1$expt2 > 20, ]
print(subset_expt2)

# Create a subset of data with only Female gender
subset_female <- dataframe1[dataframe1$Gender == "F", ]
print(subset_female)

# Create a subset of data with Male gender for which “expt2” is less than 30.
subset_male_expt2 <- dataframe1[dataframe1$Gender == "M" & dataframe1$expt2 < 30, ]
print(subset_male_expt2)


#6 

# If-else-if structure.
# Write an if-else-if structure that explores and prints the quadrant in which an
# angle belongs. For example, if you input 45o it should print ‘First quadrant’.
# Function to determine the quadrant of an angle

find_quadrant <- function(angle) {
  angle <- angle %% 360  
  
  if (angle > 0 & angle <= 90) {
    print("First quadrant")
  } else if (angle > 90 & angle <= 180) {
    print("Second quadrant")
  } else if (angle > 180 & angle <= 270) {
    print("Third quadrant")
  } else if (angle > 270 & angle < 360) {
    print("Fourth quadrant")
  } else {
    print("Angle lies on an axis or is 0/360 degrees")
  }
}

#calling the function
find_quadrant(45)  
find_quadrant(120)  
find_quadrant(360)  

# Write an if-else-if structure that takes three numeric inputs and uses this structure
# alone to put the 3 numbers in decreasing order. Do not use any built in function
# for sorting purposes.


sort_num<- function(a, b, c) {
  if (a >= b & a >= c) {
    if (b >= c) {
      print(c(a, b, c))
    } else {
      print(c(a, c, b))
    }
  } else if (b >= a & b >= c) {
    if (a >= c) {
      print(c(b, a, c))
    } else {
      print(c(b, c, a))
    }
  } else {
    if (a >= b) {
      print(c(c, a, b))
    } else {
      print(c(c, b, a))
    }
  }
}

#sorting all the numbers in descending order
sort_num(15, 9, 20) 

# Let’s say the cost of a journey ticket depends not only on the distance traveled
# but also on the details of the traveler. Distance-wise, the cost is a minimum of Rs.
# 100 for the first 100km, Rs. 1.50 for every extra km until 1000km and Rs.2 per
# km thereafter. On top of that, senior citizens (> 60 years ) get a 25% concession
# and children under 6 years of age get 50% concession. Write a code that takes the
# journey distance and the traveller’s age as inputs, and prints out the ticket cost.


ticket_cost <- function(d, age) {
  if (d <= 100) {
    cost <- 100
  } else if (d <= 1000) {
    cost <- 100 + (d - 100) * 1.50
  } else {
    cost <- 100 + (900 * 1.50) + (d - 1000) * 2
  }
  

  if (age > 60) {
    cost <- cost * 0.75  
  } else if (age < 6) {
    cost <- cost * 0.50  
  }
  
  print(paste("Ticket cost: Rs.", round(cost, 2)))
}

ticket_cost(500, 65)  # Senior citizen discount 
ticket_cost(1200, 5)  # Child discount 
ticket_cost(2000, 40) # Normal fare for 2000 km


#7

#Write a function to replace all the negative values in a vector by zeros.

replace <- function(vec) {
  vec[vec < 0] <- 0
  return(vec)
}

v <- c(-5, 10, -3, 8, -1, 7)
replace(v)  



#Write a function to calculate the factorial of a number using the Stirling’s approximation: #couldn't copy the whole question due to format
 
stirling <- function(n) 
  {
  pi <- sqrt(2 * pi * n)
  power<- (n / exp(1))^n
  corr<- 1 + (1 / (12 * n)) + (1 / (288 * n^2)) - (139 / (51840 * n^3)) - (571 / (2488320 * n^4))
  return(pi * power* corr)
}

stirling(5) 


#Write a function to sum the digits of a number.
sum_digits <- function(num) {
  total <- 0
  while (num > 0) {
    total <- total + (num %% 10)
    num <- num %/% 10  
  }
  return(total)
}

sum_digits(9876)  




































#JN
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



