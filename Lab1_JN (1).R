#Lab1
############# Q1 ################################################
print(2.7/2)
print(2.7 %/% 2) #gives quotient
print(2.7 %% 2) #gives remainder
print((10+5*1i)/2) #alternate is (10+5i)/2
print(round(2.5))
print(round(-2.5)) #prints the nearest integer 
print(round(-3.2))
print(round(-3.7))
print(2%/%4 - 1) #2%/%4 is 0
print(3*2**2) #same as 3*2^2
print(3**2*2)
print(7%/%4)
print(7%%4)
print(-7%%4) #gives 1 because the closest multiple of 4 is -8 and -8+1 = -7
print(trunc(5.7))
print(trunc(-5.7))

#######################  Q2 #######################################
print(ceiling(5.7))
ceil_alt <- function(x){floor(x+0.5)}
print(paste('Alternate ceiling function result: ',ceil_alt(5.7)),quote=FALSE)

####################### Q3 #######################################
a <- 1
b <- 2
c <- 4
print(a&b) # why does this return TRUE? Because anything nonzero is TRUE.
print(! a<b | c>b) #follows left to right operation order

###################### Q4 ########################################
x <- c(5,3,7,8)
print(x)
print(is.integer(x)) #we get FALSE because all numbers are default type 'double' ie float.
print(typeof(1)) #will return 'double' so is.integer(1) will be FALSE
print(is.numeric(x))
x <- integer(length(x))
print(x)
x <- c(5,3,7,8)
x <- as.integer(x)
print(is.integer(x)) #This returns TRUE because x is now coerced to be an integer array

###################### Q5 #######################################
x <- sqrt(2)
print(x*x==2)
print(x*x - 2)
print(all.equal(x*x,2))
