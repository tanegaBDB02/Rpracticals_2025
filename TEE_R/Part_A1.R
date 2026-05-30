#Part A-A1

lazy_caterer <- function(n) {
  if (n == 0) return(1)
  if (n <= 2) return(n)
  result <- lazy_caterer(n-1)+n
  return(result)
}

#defining the closed form of the sequence
closed_form <- function(n) {
  result <- (n^2+n+2)/2
  return(result)
}

#testing for a few values
for (i in c(0, 1:5)) {
  print(paste("lazy_caterer(",i,") =", lazy_caterer(i)))
  print(paste("closed form:", closed_form(i), "\n"))
}
