#5.1
# Manipulating the ‘factors’ in existing data
# (i) Change the current class of the headers in the Brain Cancer data to the class
# ‘factor’ and check that it is indeed the factor class.
data=read.csv(file="/home/ibab/R/BrainCancer.csv",header=TRUE)
data$sex<- factor(data$sex,levels=c("Male","Female"))
is.factor(data$sex)
#5.2 Print the number of levels in the category ’Sex’ using nlevels() function
print(nlevels(data$sex))
#5.3Print the levels in the categrory ’Diagnosis’ using the levels() function
print(levels(data$Diagnosis))

#6.1(i) Generate a new category called ’Temperature’ containing the same number of
# elements are number of rows in the Brain Cancer data with labels ’Hot’, ’Cold’
# and ’lukewarm’.
num_rows <- nrow(data)
temperature_factor <- gl(n = 3, k = ceiling(num_rows / 3), length = num_rows, labels = c("Hot", "Cold", "Lukewarm"))
print(head(temperature_factor))

#alt way and the right way
num_rows <- dim(data)[1]  # Get number of rows
temp <- gl(n = 3, k = ceiling(num_rows / 3), length = num_rows, labels = c("Hot", "Cold", "Lukewarm"))
print(temp)

#6.2Add this category of data to the above Brain Cancer dataframe and give it a new
# dataframe name. Print this new dataframe. 
#this one is wrong
brain_cancer_new <- data  
brain_cancer_new$Temperature <- temperature_factor  
print(brain_cancer_new)  
#alt way and the right way ..also continued for the right version
new_data <- data.frame(data, Temperature = temp)

#7.1
tapply(data$gtv, data$ki, mean)
#7.2
tapply(data$gtv, data$ki, mean, trim=0.1) #trim removes extreme values


#8Finding parallel-max and min. Use the pmin() and pmax() functions to find the mini-
# mum for each triplet set of 3 vectors data$gtv, data$ki and data$time
print(pmin(data$gtv,data$time,data$ki))
print(pmax(data$gtv,data$time,data$ki))
#x1<- c(1,2,9,7,10)
#x2<- c(4,76,64)
#print(pmin(x1,x2))

#9.1
#differnence between rank, sort, and order
ranks<- rank(data$gtv)
sorted<- sort(data$gtv)
ordered<- order(data$gtv)
view<- data.frame(data$gtv, ranks, sorted, ordered)
print(view)

#9.2Print the diagnosis column using the indices from the ordered vector and create
# a new dataframe with the data$gtv and data$diagnosis columns. Write this
# dataframe to a new csv file called ’lab4 ordered data.csv’.
# print(data$diagnosis[ordered])
ordered_data <- data.frame(gtv = data$gtv[ordered], diagnosis = data$diagnosis[ordered])
write.csv(ordered_data, "lab4_ordered_data.csv", row.names = FALSE)
print(head(ordered_data))

#alt way
diagnosis_sorted=data$diagnosis[ordered]
new_data <- data.frame(gtv = sorted, diagnosis = diagnosis_sorted)
write.csv(new_data, "lab6_ordered_data.csv", row.names = FALSE)


#10.1Extract the following rows and columns from the Brain cancer data: rows 1-6,
# columns 3-8
filter1=data[1:6,3:8]
print(filter1)
#10.2Convert the class of this object into a matrix using as.matrix() function and
# check the attributes to make sure it is a matrix
filter2=as.matrix(filter1)
print(attributes(filter2))

#add these as well
filter_mat=as.matrix(fil)
print(class(filter_mat))
print(mode(filter_mat))
print(attributes(filter_mat))

#10.3Create a newcol vector/column by adding 3 vectors - data$ki, data$gtv, data$time.
newcol=data$ki+data$gtv+data$time

#10.4Create a new dataframe by adding this column to the last, call this new dataframe
# as newcoladded. Print the column names and make sure that newcol has been
# added.
newcoladded=data.frame(data,newcol)
print(newcoladded)
print(colnames(newcoladded))

#10.5One can add columns in another way, using cbind(). Use cbind() to add newcol
# to the original data dataframe. Call this new dataframe newcoladded2. Print
# the column names to make sure the new column has been added.
newcol <- data$ki + data$gtv + data$time
newcoladded2 <- cbind(data, newcol)
print(newcoladded2)

#10.6Pick rows 26 and 35 from the original data and add these are new rows to the
# original data using the data.frame() function. Print the new dataframe and
# make sure that the rows have been added. Also print the dimensions of the newly
# created dataframe.
new_rows <- data[c(26:35),] 
newdata <- rbind(data, new_rows)
print(newdata)
print(dim(newdata))


#11 Adding row and column names. Create a 4x5 matrix with the following data.
# (1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0). Then add rownames to this matrix
# using the rownames command where the row numbers are prefixed by ’Trial-’. Sim-
#   ilarly, add column names to this matrix by (a) supplying 5 different names as “as-
#   pirin”,“paracetamol”,“nurofen”,“hedex”,“placebo”, and also by (b) using the dimnames()
# function where each column number is prefixed by “drug.”

X <- matrix(c(1,0,2,5,3, 1,1,3,1,3, 3,1,0,2,2, 1,0,2,1,0), nrow=4, byrow=TRUE)
print(rownames(X)) 
print(colnames(X))
rownames(X) <- paste("Trial", 1:4, sep=" ")
colnames(X) <- c("aspirin", "paracetamol", "nurofen", "hedex", "placebo")
print(X)

dimnames(X) <- list(paste("Trial", 1:4, sep=" "), paste("drug", 1:5, sep=" "))
print(X)

#alt_way
x<-matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0),nrow=4)
print(x)
print(rownames(x))
print(colnames(x))
rownames(x)<-rownames(x,do.NULL = FALSE,prefix='trial.')
drugs<-c("aspirin","paracetamol","nurofen","hedex","placebo")
colnames(x)<-drugs
print(x)
dimnames(x) <- list(rownames(x), paste("drug", 1:5, sep=""))
print(x)# prints as trial1,2,3...
dimnames(x) <- list(NULL, paste("drug", 1:5, sep=""))
print(x)#no row names coz it's null

#12.1 mean of the column 5
mean(X[,5])

#12.2Variance of the 4th row
var(X[4,])

#12.3 Row Sums
rowSums(X) 
apply(X,1,sum)

# Alternative method
print("Row Sums (apply function):")
print(apply(X, 1, sum))


#12.4Column Sums
colSums(X)
apply(X,2,sum)

# Alternative method
print("Column Sums (apply function):")
print(apply(X, 2, sum))

#12.5 Row Means
rowMeans(X)
apply(X,1,mean)

# Alternative method
print("Row Means (apply function):")
print(apply(X, 1, mean))

#12.6 Column Means
colMeans(X)
apply(X,2,mean)

# Alternative method
print("Column Means (apply function):")
print(apply(X, 2, mean))

# #12.7 Sum groups of rows within a column. rowsum(X,c("A","B","B","A")). Another
# method is to use the tapply() or aggregate() function:
#   (a) group=c("A","B","B","A")
# (b) what do row(X) and col(X) do? As an alternative to rowsum do the fol-
#   lowing.
# (c) tapply(X,list(group[row(X)], col(X))),sum)
# (d) aggregate(X,list(group),sum)

group <- c("A", "B", "B", "A")
rowsum_result <- rowsum(X, group)
print(rowsum_result)
#print row indices
print(row(X))  
#print column indices
print(col(X))
tapply_result <- tapply(X, list(group[row(X)], col(X)), sum)
print(tapply_result)
aggregate_result <- aggregate(X, list(group), sum)
print(aggregate_result)

#12.8Shuffling the elements of a column: apply(X,2,sample). How do we shuffle elements of a row?

apply(X,2,sample)

#see this
X2<-matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0),nrow=4)
print(X2)
Y<-apply(X2,2,sample) #column wise shuffling
print(Y)
Y1<-apply(X2,1,sample) #row wise shuffling
print(t(Y1))

#12.9Add a row at the bottom of matrix X showing the column means, and a column at
# the right showing the row variances. X <- rbind(X, apply(X,2,mean)) will add
# the row at the bottom, and X <- cbind(X,apply(X,1,var)) will add a column
# at the right showing the row variances. Also add headings to the last column and
# row as follows: headings <- c(paste("drug.",1:5,sep=""),"var") followed
# by dimnames(X)<- list(NULL,headings) and
# headings <- c(paste("Trial-",1:4,sep=’’),"Mean") followed by
# rownames(X) <- headings. Print the X matrix now to make sure all the above
# changes have been made, and it looks as thus:

X <- rbind(X, apply(X,2,mean))
X <- cbind(X,apply(X,1,var))
headings <- c(paste("drug.",1:5,sep=""),"var") 
dimnames(X)<- list(NULL,headings) 
headings <- c(paste("Trial-",1:4,sep=''),"Mean") 
rownames(X) <- headings
print(X)


#13
#sweep() function
#to perform sweep action
# Create a dataframe containing the columns data$ki, data$gtv and data$time.
# Call this dataframe as eg_sweep.
data=read.csv("/home/ibab/R/BrainCancer.csv", header=TRUE)
eg_sweep = data.frame(data$ki,data$gtv,data$time)

# Next we need to create a vector containing the parameters that you intend to
# sweep out. For example let’s say we want column means. Use the apply()
# function to obtain a vector called cols that contains the mean of each column.
cols <- apply(eg_sweep,2,mean)
print(cols)

# Let’s first do the sweeping operation without using sweep() function, and then
# use sweep() to perform the same thing. Execute the following two commands:
#   cols.means <- matrix(rep(cols,rep(dim(eg_sweep)[1],dim(eg_sweep)[2])),
#                        nrow=dim(eg_sweep)[1])
# print(cols.means)
# eg_sweep_alt <- eg_sweep - cols.means
# print("Method 1")
# print(eg_sweep_alt)
cols.means <- matrix(rep(cols,rep(dim(eg_sweep)[1],dim(eg_sweep)[2])),
                     nrow=dim(eg_sweep)[1])
print(cols.means)
eg_sweep_alt <- eg_sweep - cols.means
print("Method 1")
print(eg_sweep_alt)

eg_sweep_alt2 <- sweep(eg_sweep,2,cols)
print("Method 2")
print(eg_sweep_alt2)

#sapply() used for vectors
eg_sapply <- sapply(3:7,seq)
print(attributes(eg_sapply))


#14:Using the max.col function. Read the data present in pgfull.txt. Extract columns
# 1:54 from the read data and call the subset data as species. max.col(species) re-
#   turns the column index of the max value in each row; print this. We will use these
# indices to collect the names of the species as names(species)[max.col(species)].
# We then use the table function to build a frequency table of each of the species as
# table(names(species)[max.col(species)]. It turns out that there is no min.col()
#       function in R, and one needs an alternative. How can max.col be used to extract the
#       column indices that give the minimum value along each row?

data <- read.table("/home/ibab/Downloads/pgfull.txt", header = TRUE)
species <- data[, 1:54]
max_indices <- max.col(species)
print(max_indices)
species_names <- names(species)[max_indices]
print(species_names)
species_freq <- table(species_names)
print(species_freq)
min_indices <- max.col(-species)  #Negating the values in 'species'
print(min_indices)
#alt method:
min_indices <- apply(species, 1, which.min)
print(min_indices)


#15
apples <- c(4,4.5,4.2, 5.1,3.9)
oranges <- c(TRUE,TRUE,FALSE)
chalk <- c("limestone","marl","oolite","CaCO3")
cheese <- c(3.2-4.5i,12.8+2.2i)
items <- list(apples, oranges, chalk, cheese)
print(items)
#subscripts on lists have double square brackets
print(items[[3]])
print(items[[3]][3])

items[3] #this works
items[3][3] #this doesnt
print(names(items))
items <- list(first=apples,second=oranges,third=chalk,fourth=cheese)
print(items$fourth)
print(class(items))
print(lapply(items,length))
print(lapply(items,class))
print(lapply(items,mean))






















#question_paper
#(a) Print column names
print(colnames(expression_data))

# (b) Compute means and ratio column
expression_data$control_mean <- rowMeans(expression_data[, 2:4])
expression_data$treatment_mean <- rowMeans(expression_data[, 5:7])
expression_data$RatioCol <- expression_data$treatment_mean / expression_data$control_mean

# Print first 10 elements of RatioCol
print(expression_data$RatioCol[1:10])

# (c) Generate four plots
par(mfrow = c(2,2))  # Divide canvas into 2x2
hist(expression_data$control3, main="Histogram of Control3", col="blue")
plot(expression_data$control1, expression_data$control3, main="Scatter Plot: Control1 vs Control3")
plot(expression_data$control3, expression_data$treatment1, main="Scatter: Control3 vs Treatment1")
plot(expression_data$control2, expression_data$treatment2, main="Scatter: Control2 vs Treatment2")






# (a) Create subset where control1 > 50
sub1 <- subset(expression_data, control1 > 50)

# (b) Select specific genes into sub2
sub2 <- subset(expression_data, gene %in% c("gene20", "gene37", "gene100"))

# (c) Randomly sample 20 rows (assuming `sample()` instead of `rnorm`)
set.seed(42)
samp <- expression_data[sample(nrow(expression_data), 20), ]
print(samp)



# (a) Generate 1000 Gaussian samples using Box-Muller Transform (instead of rnorm)
box_muller <- function(n, mean, sd) {
  u1 <- runif(n)
  u2 <- runif(n)
  z1 <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
  return(mean + sd * z1)
}

set.seed(42)
gaussian_data <- box_muller(1000, 20, 4)
hist(gaussian_data, main="Gaussian Histogram", col="lightblue")

# (b) Compute Full Width at Half Maximum (FWHM)
fwhm <- function(data) {
  density_data <- density(data)
  max_y <- max(density_data$y)
  half_max <- max_y / 2
  indices <- which(density_data$y >= half_max)
  width <- max(density_data$x[indices]) - min(density_data$x[indices])
  return(width)
}
print(paste("FWHM:", fwhm(gaussian_data)))

# (c) Compute area under Gaussian curve between x=35 and x=50 (without pnorm)
normal_pdf <- function(x, mean, sd) {
  (1 / (sd * sqrt(2 * pi))) * exp(-((x - mean)^2) / (2 * sd^2))
}

trapezoidal_integration <- function(f, a, b, mean, sd, n = 1000) {
  x_vals <- seq(a, b, length.out = n)
  y_vals <- sapply(x_vals, f, mean = mean, sd = sd)
  dx <- (b - a) / (n - 1)
  area <- sum((y_vals[-1] + y_vals[-length(y_vals)]) * dx / 2)
  return(area)
}

area <- trapezoidal_integration(normal_pdf, 35, 50, 30, 6)
print(paste("Area under curve:", area))


# (a) Generate binomial distribution manually (instead of rbinom)
binomial_distribution <- function(n, p, size) {
  results <- rep(0, size)
  for (i in 1:size) {
    results[i] <- sum(runif(n) < p)  # Simulating binomial trials
  }
  return(results)
}

binom1 <- binomial_distribution(12, 0.3, 1000)
binom2 <- binomial_distribution(13, 0.7, 1000)

hist(binom1, col="blue", main="Binomial Distributions")
hist(binom2, col="red", add=TRUE)

# (b) Generate Poisson distribution manually (instead of rpois)
poisson_manual <- function(lambda, size) {
  results <- rep(0, size)
  for (i in 1:size) {
    L <- exp(-lambda)
    k <- 0
    p <- 1
    while (p > L) {
      k <- k + 1
      p <- p * runif(1)
    }
    results[i] <- k - 1
  }
  return(results)
}

poisson_data <- poisson_manual(8, 5000)
hist(poisson_data, col="lightblue", main="Poisson Distribution")

# Overlay a Gaussian curve with same mean & standard deviation
mean_pois <- mean(poisson_data)
sd_pois <- sd(poisson_data)

x_vals <- seq(min(poisson_data), max(poisson_data), length.out = 100)
y_vals <- dnorm(x_vals, mean=mean_pois, sd=sd_pois) * length(poisson_data) * diff(hist(poisson_data, plot=FALSE)$breaks)[1]
lines(x_vals, y_vals, col="red", lwd=2)


# Data for ANOVA
strain1 <- c(10, 12, 9, 11, 10, 12)
strain2 <- c(14, 15, 14, 16, 14, 15)
strain3 <- c(9, 8, 10, 11, 10, 10)
strain4 <- c(12, 11, 13, 14, 12, 13)

# Combine into a data frame
anova_data <- data.frame(
  value = c(strain1, strain2, strain3, strain4),
  strain = rep(c("strain1", "strain2", "strain3", "strain4"), each=6)
)

# Perform ANOVA
anova_result <- aov(value ~ strain, data=anova_data)
summary(anova_result)

# Print results
print("Null Hypothesis: The mean effect of the 4 different strains is the same.")
print(paste("F-statistic:", summary(anova_result)[[1]]$`F value`[1]))
print(paste("p-value:", summary(anova_result)[[1]]$`Pr(>F)`[1]))

# Conclusion
if (summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.04) {
  print("Conclusion: Reject the null hypothesis. At least one strain mean is significantly different.")
} else {
  print("Conclusion: Fail to reject the null hypothesis. No significant difference.")
}

# (b) Boxplot comparison
boxplot(value ~ strain, data=anova_data, main="Boxplot of Strains", col=c("red", "blue", "green", "purple"))



#alt way
#ex3
# (i) Generating random deviates and plotting the histogram
data_random <- rnorm(10000, mean = 20, sd = 4)
hist(data_random, main = "Histogram of Gaussian Random Deviates", xlab = "Value")

# (ii) Computing Full Width at Half Maximum (FWHM)
half_max <- max(density(data_random)$y) / 2
fwhm <- diff(range(density(data_random)$x[density(data_random)$y >= half_max]))
print(fwhm)

# (iii) Area under the curve between x=35 and x=50 for N(30, 6)
library(stats)
area <- pnorm(50, mean = 30, sd = 6) - pnorm(35, mean = 30, sd = 6)
print(area)
##
#
#ex4
# (i) Plotting two binomial distributions
x1 <- 0:12
x2 <- 0:13
binom1 <- dbinom(x1, size = 12, prob = 0.3)
binom2 <- dbinom(x2, size = 13, prob = 0.7)
plot(x1, binom1, type = "h", col = "blue", ylim = c(0, max(c(binom1, binom2))),
     main = "Binomial Distributions", xlab = "X", ylab = "Probability")
lines(x2, binom2, type = "h", col = "red")

# (ii) Poisson distribution and overlaying Gaussian curve
poisson_data <- rpois(5000, lambda = 8)
hist(poisson_data, freq = FALSE, breaks = 30, main = "Poisson vs Gaussian", xlab = "Value")
curve(dnorm(x, mean = mean(poisson_data), sd = sd(poisson_data)),
      col = "blue", add = TRUE)


