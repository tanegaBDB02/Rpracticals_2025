#PART A- A1

is_harshad <- function(n) {
  #calculate the sum of digits
  sum_of_digits <- sum(as.numeric(strsplit(as.character(n),"")[[1]]))
  
  #checking if n is divisible by the sum of its digits
  if (n%%sum_of_digits==0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_harshad(21) 
is_harshad(41) 



