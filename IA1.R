# ===== Question 1 =====
# Write a code to sum up the series Sn = sum(2^-n) until |2 - Sn| < epsilon.
# Define an epsilon and check what value of n is needed to achieve the desired convergence.
# [3 marks]
epsilon <- 1e-6 
Sn <- 0
n <- 1

repeat {
  Sn <- Sn + 2^(-n)
  if (abs(2 - Sn) < epsilon) {
    break
  }
  n <- n + 1
}

cat("Sum Sn:", Sn, "\n")
cat("Value of n required:", n, "\n")


# ===== Question 2 =====
# Write a code to get the Fibonacci numbers beginning with f0 = 0 and f1 = 1 up to f25.
# Note: You'll have to think about how to swap numbers. In case you are not familiar,
# Fibonacci numbers are those belonging to the Fibonacci series where each number is
# the sum of the previous two numbers. For example, 1,2,3,5,8,... In this question,
# the Fibonacci series will start from the numbers 0,1 etc.
# [5 marks]
fib <- numeric(26)  # Vector to hold f0 to f25
fib[1] <- 0
fib[2] <- 1

for (i in 3:26) {
  fib[i] <- fib[i - 1] + fib[i - 2]
}

cat("Fibonacci numbers f0 to f25:\n")
print(fib)


# ===== Question 3 =====
# We know what the factorial function does and by now you should have coded how to
# calculate the factorial function n!. The double factorial function n!! is the product
# of positive odd integers up to and including n (which must itself be odd):
#
# n!! = Π_{i=1}^{(n+1)/2}(2i - 1)   [Equation 1]
#
# which basically calculates 1 · 3 · 5 · 7 · (n - 2) · n. Write a function to calculate
# the double factorial in R, and print the result for n = 11. Extend this code to also
# allow calculation of the double factorial for even values of n, using the formula:
#
# n!! = Π_{i=1}^{n/2}(2i)           [Equation 2]
#
# where the product 2 · 4 · 6 · (n - 2) · n is calculated.
# [7 marks]
double_factorial <- function(n) {
  if (n <= 0) return(1)
  prod_seq <- seq(n, 1, by = -2)
  result <- prod(prod_seq)
  return(result)
}

# Test for n = 11 (odd)
n1 <- 11
cat("Double factorial of", n1, "(odd):", double_factorial(n1), "\n")

# Test for n = 10 (even)
n2 <- 10
cat("Double factorial of", n2, "(even):", double_factorial(n2), "\n")

