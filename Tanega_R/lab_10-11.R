# (1) Sampling from a vector: One can sample from a set of items with or wihout replace-
#   ment. Test the result of the following commands:
#   
#   x <- seq(1,100)
#   s <- sample(x,10)
#   Is the above result happening with or without replacement?
#     Try the command sample(x,10,replace=TRUE)

x<- seq(1,100)
s<- sample(x,10)
print(x)
print(s) #samples 10 elements from x

sample(x,10,replace=TRUE) #samples with replacemtn
sample(x,10,replace=FALSE) #samples withput replacement

# (2) The package gtools has permutations and combinations functions that should be used
# as follows.
# install.packages("gtools")
# library(gtools)
# x <- c("A","B","C","D")
# per <- permutations (n=length(x), r=3,v=x,repeats.allowed=TRUE)
# print(per)
# comb <- combinations(n=length(x), r=3, v=x)
# where r is the size of the target vector, v is the source vector, n is the size of the source
# vector.

install.packages("gtools")

library(gtools)
x <- c("A","B","C","D")
per <- permutations (n=length(x), r=3,v=x,repeats.allowed=TRUE) #choosing three combinations from the vector of four elements
print(per)
comb <- combinations(n=length(x), r=3, v=x) #possible ordered sequences
print(comb)


# (1) Binomial distribution: The shape of this PDF is decided by the parameter p. Use
# n=10, p=0.4 and m=3 to do the following:
#   (a) Print the probability value for the above combination of numbers. The syntax is
# dbinom(m,n,p).
n<-10
m<- 3
p<- 0.4
dbinom(m,n,p)

# (b) Print the cumulative probability value for the above. Syntax: pbinom(m,n,p).
cum_prob<- pbinom(m,n,p)
print(cum_prob)

# (c) Find the m value corresponding to cumulative probability of 0.8. Syntax: qbinom(cum_prob,n,p)
qbinom(cum_prob,n,p)

# (d) Print 5 points randomly sampled from the Binomial distribution using rbinom(npts,n,p).
rbinom(5,n,p)

# (e) Plot the probability density function (PDF) for the above parameters. On the same
# plot, plot the PDF for p=0.7 with a different colour.
n<-10
x_vals <- 1:n
pdf_0.4 <- dbinom(x_vals, n, 0.4)  
pdf_0.7 <- dbinom(x_vals, n, 0.7) 

par(mfrow=c(2,1))  
y_max <- max(c(pdf_0.4, pdf_0.7))

barplot(pdf_0.4, names.arg=x_vals, col="skyblue", main="Binomial PMF (p=0.4)", xlab="X", ylab="Probability",ylim=c(0, y_max))
barplot(pdf_0.7, names.arg=x_vals, col="pink", main="Binomial PMF (p=0.7)", xlab="X", ylab="Probability",ylim=c(0, y_max))

par(mfrow=c(1,1))  


# (f) Generate 100 and 10000 points randomly from this distribution and make a frequency table of the sampled points. 
#Plot these as bar plots in a 2x1 grid.
n<-10
set.seed(42) 
sample_100 <- rbinom(100, n, prob=0.4)
sample_10000 <- rbinom(10000, n, prob=0.4)

# Frequency tables
freq_100 <- table(sample_100)
freq_10000 <- table(sample_10000)

# Plot frequency distributions
par(mfrow=c(2,1))
barplot(freq_100, col="purple", main="Frequency Distribution (n=100)", xlab="x", ylab="Frequency")
barplot(freq_10000, col="violet", main="Frequency Distribution (n=10000)", xlab="x", ylab="Frequency")

# Hypergeometric distribution: The functions pertaining to this distribution <distn name>
#   hyper. The parameters are N, K, n and k (see class notes). Carry out the following:
#   (a) Plot a histogram type plot of the hypergeometric probability density function with
# N=100, K=70, p=0.3, n=12 and add text within the plot window of the parameter
# names and their values.
N<-100
K<-70
p<-0.3
n=12

k_vals <- 1:n
pdf_vals <- dhyper(k_vals, K, N-K, n)

barplot(pdf_vals, names.arg=k_vals, col="steelblue3", main="Hypergeometric PMF", xlab="X-axis", ylab="Probability")

text(2, max(pdf_vals) * 0.8, paste("N =", N, "\nK =", K, "\nn =", n), col="navy", cex=1.2)

# (b) Compute the cumulative probability up to x=10 and print the result after rounding off to 3 decimal places.

#using phyper for cumulative frequency
cum_prob_hyper <- round(phyper(10, K, N-K, n), 3)
print(cum_prob_hyper)

# (c) Obtain the x value corresponding to a cumulative probability value of 0.9.
five_points=qhyper(0.9, K, N-K, n)
print(five_points)

# (d) Sample 5 points randomly from this distribution and print these with two significant
# digits.
random_samples <- sample(x_vals, size=5, replace=TRUE, prob=pdf_vals)
print(round(random_samples, 2)) 

# (3) Geometric distribution: The functions pertaining to this distribution <distn name>
#   use geom. The parameters are p and m(trial number at which the first success is observed).
# (a) Plot 2 probability density functions for this distribution in a 1x2 grid with (i)p=0.3
# and (ii) p=0.8. What differences do you see?
par(mfrow=c(1,2))

p1=0.3
p2=0.8
x_vals=1:10

pdf1=dgeom(x_vals-1,p1)
pdf2=dgeom(x_vals-1,p2)

barplot(pdf1, names.arg=x_vals, col="salmon", main="Geometric PMF (p=0.3)", xlab="m", ylab="Probability")
barplot(pdf2, names.arg=x_vals, col="orangered", main="Geometric PMF (p=0.8)", xlab="m", ylab="Probability")

par(mfrow=c(1,1))  

#(b) Compute the cumulative probability up to x=4.
pgeom(3,0.3) #x value minus 1 that is equal to 3

# (c) Compute the value of m at which the cumulative probabilty is 0.2.
qgeom(0.2,0.3)+1 #takes failure till that

# (d) Generate 6 random deviates or sample points from this distribution with p=0.4.
random_samples=rgeom(6,0.4)
print(random_samples)


# (4) Negative binomial distribution: The functions pertaining to this distribution uses
# nbinom. The parameters are p, r (number of successes desired) and y (number of failures
#                                                                      before r successes).
# (a) Compute and print the negative binomial probability density for y=5, r=3 and
# p=0.3
y=5 #the number of failures
r=3 #target number of success
p=0.3 #probability
dnbinom(y,r,p) 

# (b) Compute and print the cumulative negative binomial probability density up to y=5.
pnbinom(y,r,p)

# (c) What is the y value corresponding to a cumulative probabilty value of 0.5? (ie the median)
qnbinom(0.5,r,p)

# (d) Print 4 random points sampled from this distribution with r=3 and p=0.3.
random_samples <- rnbinom(4,r,p)
print(random_samples)

# (e) Plot the negative binomial distribution function using r=10, p=0.3.
r=10
p=0.3
y_vals=0:65

pmf_vals=dnbinom(y_vals,r,p)
barplot(pmf_vals, names.arg=y_vals, col="slateblue4",main="Negative Binomial PMF (r=10, p=0.3)"
        ,xlab="Number of Failures (y)", ylab="Probability")

# (f) Generate a frequency histogram of 10,000 random deviates from this distribution
# with r=10 and p=0.3.
sample_10000 <- rnbinom(10000,10,0.3)

hist(sample_10000,breaks=30,col="hotpink4", 
     main="Histogram of 10,000 Negative Binomial Deviates", 
     xlab="Number of Failures (y)", ylab="Frequency", border="black")


# (5) Poisson distribution: The functions pertaining to this distribution uses pois. The
# key parameter is λ and the discrete variable being m.
# (a) Compute and print the Poisson probability given λ = 10 and m = 7.
lambda=10
m=7
dpois(m,lambda)

# (b) Calculate and print the cumulative probability for the same values above.
ppois(m,lambda)

# (c) Make two barplots showing a binomial probability distribution with n = 1000, p =0.3 and a Poisson PDF with λ = np.
#Do the two distributions agree? Why? Why not?
n <- 4500
p <- 0.01
lambda <- n * p  

x_vals <-10:80 

binom_prob <- dbinom(x_vals, n, p)
poiss_prob <- dpois(x_vals, lambda)

plot(x_vals, binom_prob, type = "h", col = "lightpink3", lwd = 1.5, 
     main = "Binomial vs Poisson Approximation",
     xlab = "X", ylab = "Probability", xlim = c(10, 80), 
     ylim = range(c(binom_prob, poiss_prob)))

lines(x_vals, poiss_prob, col = "darkred", pch = 16)

legend("topright", legend = c("Binomial", "Poisson"), 
       col = c("lightpink3", "darkred"), pch = c(124, 16), lwd = 1.8)



#(d) Find the quantile value corresponding to cumulative probability of 0.22 and λ = 10.
lambda=10
qpois(0.22, lambda)

#(e) Obtain 10000 random sample points from a Poisson distribution with λ = 9 and make a histogram plot.
# Create histogram
library(ggplot2)
samples <- rpois(10000, 9)

ggplot(data.frame(samples), aes(x = samples)) +
  geom_histogram(binwidth = 1, fill = "palegreen3", color = "black", alpha = 0.7) +
  ggtitle("Histogram of Poisson Samples (λ=9)") +
  xlab("Value") + ylab("Frequency")



# (6) Gaussian distribution: The functions in R for the Gaussian/Normal distribution are
# for the unit normal distribution with the suffix norm. As we know the relevant parameters
# are μ and σ.
# (a) Compute and print the unit normal PDF value for μ = 12 and σ = 2.
mean=12
sd=2
x=10
dnorm(x,mean,sd)

# (b) Calculate and print the cumulative probability for Z = 2.0. Is this same as 1-CPDF(Z=-2)?
Z=2.0
cdf_value <- pnorm(Z, mean = 0, sd = 1)
cdf_neg2 <- pnorm(-2, mean = 0, sd = 1)
check_identity <- 1 - cdf_neg2

print(cdf_value)
print(check_identity)

#yes it is the same value hence it confirms the identity (the distribution is symmetric)

#   (c) Plot a unit normal curve for the above parameters with X range of ±4σ and add a
# text box to the plot showing the parameter symbols and their values.
mean=12
sd=2
x_vals=seq(mean-4*sd,mean+4*sd,length = 100)
y_vals=dnorm(x_vals,mean,sd)

plot(x_vals, y_vals, type = "l", col = "navy", lwd = 1.8, 
     main = "Normal Distribution (μ=12, σ=2)",
     xlab = "X", ylab = "Density")

text(mean, max(y_vals) * 0.8, labels = expression(paste(mu, " = 12, ", sigma, " = 2")), col = "royalblue3")

# (d) Generate the 75th quantile point for a unit normal distribution with the above
# parameters.
qnorm(0.75,mean,sd)

# (e) Generate 10,000 random deviates from the unit normal distribution and plot a
# histogram. On top of this plot the unit normal curve from which you sampled.
normal_samples <-rnorm(10000, mean,sd)

hist(normal_samples, breaks = 50, probability = TRUE, col = "lightgoldenrod3", 
     main = "Histogram of Normal Deviates", xlab = "X")

lines(x_vals, y_vals, col = "sienna", lwd = 2)  

# (f) Make a histogram plot of a ‘normalised’ binomial distribution with μ = np = 10
# and p = 0.5. ‘Normalised’ here means computing a variable of type W = √m−np/np(1−p)
# where m is the number of successes in n trials. On top of this, plot a unit normal
# distribution N(np,np(1-p). Do the two distributions agree?
n <- 10000
p <- 0.5
mu_binom <- n*p
sigma_binom <- sqrt(n *p*(1-p))

binom_samples <- rbinom(10000,n,p)
W <- (binom_samples - mu_binom) / sigma_binom

hist(W, breaks ="Sturges", probability = TRUE, col = "turquoise3", 
     main = "Normalized Binomial vs. Normal Distribution", xlab = "W")

curve(dnorm(x, mean=0,sd=1), add = TRUE, col = "violetred3", lwd = 2)

# (g) Plot 4 Poisson probability distribution functions corresponding to λ values 1, 10,
# 100 and 1000 in the same graph. On the same graph of each Poisson PDF plot,
#  plot a unit normal distribution with Z = m−λ/λ. For what value of λ(s) do the two plots agree?
#Use a 2x2 grid for this problem.
library(ggplot2)
lambda_values <- c(1, 10, 100, 1000)

par(mfrow=c(2,2))

for (lam in lambda_values) {
  x_poisson <- 0:ceiling(lam + 3 * sqrt(lam))
  poisson_pmf <- dpois(x_poisson, lam)
  
  x_normal <- seq(min(x_poisson), max(x_poisson), length=100)
  normal_pdf <- dnorm(x_normal, mean=lam, sd=sqrt(lam))
  
  plot(x_poisson, poisson_pmf, type="h", lwd=2, col="darkseagreen4",
       main=paste("Poisson vs Normal (λ=", lam, ")", sep=""),
       xlab="x", ylab="Probability", ylim=c(0, max(poisson_pmf, normal_pdf)))
  
  lines(x_normal, normal_pdf, col="deeppink4", lwd=2)
  
}
par(mfrow=c(1,1))

# (h) The library MASS is used to generate two or more vectors of normally distributed
# random numbers that are correlated with one another to a specified degree. For
# example, xy <- mvrnorm(1000, mu=c(50,60), matrix(c(4,3.7,3.7,9),2))
# will generate two sets of 1000 numbers each with a mean of 50 for the first set and
# 60 for the second set. The matrix option specifies the covariance matrix of the
# variables.
# (i) Execute var(xy) to check how close the variances are to our specified values
# – what is the covariance from these sample sets?
install.packages("MASS")
library(MASS)
xy <- mvrnorm(1000, mu=c(50,60), matrix(c(4,3.7,3.7,9),2))
var(xy)

# (ii) Extract the separate vectors x and y as x <- xy[,1] and y <- [xy,2] and
# plot them to look at the correlation. Print the individual variances as var(x)
# and var(y).
x <- xy[,1]
y <- xy[,2]
print(var(x))
print(var(y))
plot(x, y, main="Plot of X vs Y", xlab="X", ylab="Y", col="deeppink3", pch=16)

# (iii) Are the two samples independent? If so then the sum of their variances
# should be equal to the variance of their sum.
var_sum <- var(x)+ var(y)
var_xy_sum <- var(x+y)

print(paste("Sum of individual variances:", var_sum))
print(paste("Variance of (X + Y):", var_xy_sum))

#taking smaller value to check if the value is close to zero
if (abs(var_sum - var_xy_sum) < 0.001) {
  print("The two samples are independent.")
} else {
  print("The two samples are dependent.")
}

# (iv) The reported covariance is var(xy). Compute the covariance using the
# correlation coefficient cor(x,y) and the individual variances and make sure
# this matches with the reported value. 
cor_xy <- cor(x, y)
computed_covariance <- cor_xy * sqrt(var(x)* var(y))

sample_var_cov=var(xy)
print(paste("Reported covariance:", sample_var_cov[1,2]))
print(paste("Computed covariance using correlation and variances:", computed_covariance))

if (abs(sample_var_cov[1,2] - computed_covariance) < 0.01) {
  print("The computed covariance matches the reported covariance.")
} else {
  print("There is a discrepancy in the covariance calculation.")
}


# (7) Uniform distribution: The function prefix here unif and the two parameters are the
# x-limits.
# (a) Generate 5 uniform random numbers between 0 and 1 and print them.
runif(5,min=0,max=1)

# (b) Generate 5 random samples from a uniform distribution between 50 and 100.
runif(5,min=50,max=100)

# (c) Generate 10,000 uniform deviates and plot a histogram with x-limits 1 and 2.
deviates=runif(10000,min=1,max=2)

hist(deviates,breaks=30,col="palevioletred2",border='black',
     main="Plot of uniform distribution (1,2)", xlab="value",freq=FALSE,xlim=c(1,2))


# (8) Exponential distribution: Since this is derived from the Poisson distribution, the
# primary parameter here is λ. The function suffix in R is exp.
# (a) What is the probability density corresponding to x = 3 and λ = 2?
dexp(3,rate=2)

#   (b) What is the quantile value corresponding to cumulative probability value of 0.995
# for the above distribution?
qexp(0.995,rate=2)

#   (c) Plot the exponential cumulative probability distributions on the same graph for
# λ = 2, 10 and 100.
# Define x values
x <- seq(0, 1, length=100)

#computing CDF values for different λ
y1 <- pexp(x, rate = 2)
y2 <- pexp(x, rate = 10)
y3 <- pexp(x, rate = 100)

plot(x, y1, type="l", col="steelblue3", lwd=3, ylim=c(0,1),
     xlab="x", ylab="Cumulative Probability", main="Exponential CDFs")
lines(x, y2, col="tomato3", lwd=3)
lines(x, y3, col="orchid3", lwd=3)

#adding legend
legend("bottomright", legend=c("λ = 2", "λ = 10", "λ = 100"),
       col=c("steelblue3", "tomato3", "orchid3"), lwd=2)

# (d) Compute and print 4 random deviates from an exponential distribution with λ = 3.
rexp(4,rate=3)


# (9) Gamma distribution: The function suffix in R is gamma. There are two parameters α
# (shape option) and θ(scale option).
# (a) Plot the PDFs on the same graph with alpha values of 1,2,3,4 and θ value 4 with
# colors black, blue, red and magenta respectively. This is one of the two graphs on
# a 1x2 grid. Plot the second graph with θ values of 1,2,3,4 and α = 4 with colors
# black, blue, red and magenta respectively.
par(mfrow=c(1,2))

x<- seq(0,50,length=1000)

#first plot
plot(x, dgamma(x,shape=1,scale=4),type='l',col="black",lwd=2,xlab="X",ylab="Density",main="Gamma Distribution with varying alpha")
lines(x, dgamma(x,shape=2,scale=4),type='l',col="blue",lwd=2)
lines(x, dgamma(x,shape=3,scale=4),type='l',col="red",lwd=2)
lines(x, dgamma(x,shape=4,scale=4),type='l',col="magenta",lwd=2)

legend("topright", legend=c("alpha=1", "alpha=2", "alpha=3", "alpha=4"), col=c("black", "blue", "red", "magenta"), lwd=2)

#second plot
plot(x, dgamma(x,shape=4,scale=1),type='l',col="black",lwd=2,xlab="X",ylab="Density",main="Gamma Distribution with varying theta")
lines(x, dgamma(x,shape=4,scale=2),type='l',col="blue",lwd=2)
lines(x, dgamma(x,shape=4,scale=3),type='l',col="red",lwd=2)
lines(x, dgamma(x,shape=4,scale=4),type='l',col="magenta",lwd=2)

legend("topright", legend=c("theta=1", "theta=2", "theta=3", "theta=4"), col=c("black", "blue", "red", "magenta"), lwd=2)

par(mfrow=c(1,1))

# (b) Compute and print the probability density corresponding to x = 6, α = 4 and
# θ = 1.
dgamma(6,shape=4,scale=1)

# (c) Compute and print the cumulative probablity up to x=6 for the above gamma
# PDF.
pgamma(6,shape=4,scale=1)

# (d) Compute the x value which corresponds to a cumulative probability of 0.95
qgamma(0.95,shape=4,scale=1)

# (e) Obtain 10,000 random deviates from the above gamma distribution and plot a
# histogram of this set.
samples=rgamma(10000,shape=4,scale=1)

hist(samples, breaks=50, col="plum4", border="black", probability=TRUE,
          main="Gamma distribution of 10000 random samples", xlab="Value")


# (10) Chi-square(χ2 distribution: The suffix for the functions in R for this distribution is chisq.
#     The key parameter is the degrees of freedom, r.
# (a) Plot the χ2 distribution with degree of freedom 2,3,5 and 10.
x<- seq(0,30,length=500)

plot(x, dchisq(x,df=2),type='l',col="peachpuff4",lwd=3,
     xlab="X",ylab="Density",main="Chi square distribution with different df")
lines(x, dchisq(x,df=3),type='l',col="palevioletred4",lwd=3)
lines(x, dchisq(x,df=5),type='l',col="paleturquoise4",lwd=3)
lines(x, dchisq(x,df=10),type='l',col="palegreen4",lwd=3)

legend("topright", legend=c("2", "3", "5", "10"), col=c("peachpuff4", "palevioletred4", "paleturquoise4", "palegreen4"), lwd=2)

# (b) Compute and print the probability density for x=6 and 5 degrees of freedom
dchisq(x=6,df=5)

# (c) Compute and print the cumulative probability up to x=6 and 10 degrees of freedom
pchisq(6,df=10)

# (d) Obtain the 85th quantile for this distribution for 6 degrees of freedom
qchisq(0.85,df=6)

# (e) Plot a histogram of 10,000 random deviates from this distribution with 6 degrees
# of freedom with 30 bins, red filled bars and text within the plot ”r=6” in an
# appropriate blank portion
samples=rchisq(10000,df=6)

hist(samples, breaks=30, col="red", border="black", probability=TRUE,
     main="Chi square distribution with random samples ", xlab="Value")

text(x=15, y=0.04, labels="r=6", col="black", cex=1.5)

# (f) Assuming μ = 2 and σ = 1 compute the variable Z2 = (x − μ)2/σ2 and plot the χ2.
# PDF with 1 degree of freedom.
mu <- 2
sigma <- 1
x <- seq(0,10,length.out=500) 
z_sq <- ((x-mu)^2)/sigma^2
plot(z_sq, dchisq(z_sq, df=1), type="l", col="maroon4", lwd=2, main="Chi-square PDF with df=1", 
     xlab="Z^2", ylab="Density")




    
    
    
    
    
