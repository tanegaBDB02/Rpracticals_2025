#For any input integer k and another integer input n where n/=0 write a function that prints the results of all arithmetic operations between k and n. 
op_kn <- function(k,n){
  sum_kn=k+n
  diff_kn=k-n
  mult_kn=k*n
  div_kn=k/n
  mod_kn = k%%n
  exp_kn=k^n
  intdiv_kn = k%/%n
  output=c(sum_kn,diff_kn,mult_kn,div_kn,mod_kn,exp_kn,intdiv_kn)
  return(output)
}
print(' ',quote=FALSE)
print("Q1. Printing all arithmetic operations between integers k,n where n/=0",quote=FALSE)
#print(op_kn(2,3))
result = op_kn(2,3)
print(c('Sum = ',result[1]),quote=FALSE)
print(paste('Sum = ',result[1]),quote=FALSE)
print(paste('Diff = ',result[2]),quote=FALSE)
print(paste('Product = ',result[3]),quote=FALSE)
print(paste('Quotient = ',round(result[4],digits=3)),quote=FALSE)
print(paste('Remainder = ',result[5]),quote=FALSE)
print(paste('Exponent = ',result[6]),quote=FALSE)
print(paste('Integer division = ',result[7]),quote=FALSE)

#Q2. Input real numbers a,b and c.Treating these as coefficients of the quadratic equation ax2+bx+c=0 find and print the roots.
print(' ',quote=FALSE)
a <- 1
b <- 3
c <- 2
find_quadroots <- function(a,b,c){
  discr = sqrt(b^2-4*a*c)
  root1  = (-b-discr)/(2*a)
  root2  = (-b+discr)/(2*a)
  return(c(root1,root2))
}
roots = find_quadroots(a,b,c)

print(paste('Q2.The two roots are : ',roots[1],',',roots[2]),quote=FALSE)
# What happens when we use print(c('The two roots are : ',root[1],',',root[2]),quote=FALSE)?